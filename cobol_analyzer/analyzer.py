# ====================================================================
# COBOL ANALYZER
# ====================================================================
# This is the heart of our system, now enhanced with external configuration
# and prompt management. The key improvements are separation of concerns
# and improved maintainability.

import os
import time
from pathlib import Path
from typing import Dict, List, Any, Optional
from langchain_anthropic import ChatAnthropic
from langgraph.graph import StateGraph, END
from typing_extensions import TypedDict
from .config import settings
from .models import FileInfo, FileSummary, FileType, RepositoryAnalysis, FolderStructure, ReportType
from .services.prompt_manager import PromptManager

class AnalysisState(TypedDict):
    """State object that flows through the LangGraph workflow."""
    repository_path: str
    all_files: List[dict]
    file_summaries: List[dict]
    global_summary: str
    report_type: str
    start_time: float

class COBOLAnalyzer:
    """
    Professional COBOL repository analyzer with external configuration.
    
    This class orchestrates the analysis of COBOL repositories using AI
    with external prompt management and enhanced configuration.
    """
    
    def __init__(self):
        """Initialize the analyzer with external configuration and prompt management."""
        if not settings or not settings.anthropic_api_key:
            raise Exception("ANTHROPIC_API_KEY not configured. Please check your .env file.")
        
        # Initialize the AI model with external configuration
        self.llm = ChatAnthropic(
            anthropic_api_key=settings.anthropic_api_key,
            model=settings.anthropic_model,
            temperature=settings.anthropic_temperature,
            max_tokens=settings.anthropic_max_tokens,
            timeout=settings.anthropic_timeout
        )
        
        # Initialize the prompt management system
        try:
            self.prompt_manager = PromptManager()
        except Exception as e:
            print(f"Warning: Prompt manager failed: {e}")
            print("Using fallback prompts")
            self.prompt_manager = None
        
        # Build the processing workflow
        self.workflow = self._build_workflow()
        
        # Concise initialization message
        prompt_status = "External" if self.prompt_manager else "Fallback"
        print(f"COBOL Analyzer ready: {settings.anthropic_model} | Prompts: {prompt_status}")
    
    def _build_workflow(self) -> StateGraph:
        """Build the LangGraph workflow for repository analysis."""
        workflow = StateGraph(AnalysisState)
        
        # Define the processing nodes
        workflow.add_node("scan_files", self._scan_files)
        workflow.add_node("analyze_files", self._analyze_files)
        workflow.add_node("generate_summary", self._generate_summary)
        
        # Define the processing flow
        workflow.set_entry_point("scan_files")
        workflow.add_edge("scan_files", "analyze_files")
        workflow.add_edge("analyze_files", "generate_summary")
        workflow.add_edge("generate_summary", END)
        
        return workflow.compile()
    
    async def analyze_repository(self, repository_path: str, report_type: ReportType = ReportType.BOTH) -> RepositoryAnalysis:
        """Analyze a complete COBOL repository with the specified report focus."""
        if not os.path.exists(repository_path):
            raise FileNotFoundError(f"Repository not found: {repository_path}")
        
        print(f"Starting analysis: {repository_path} ({report_type.value})")
        
        start_time = time.time()
        
        # Initialize the workflow state
        initial_state = {
            "repository_path": repository_path,
            "all_files": [],
            "file_summaries": [],
            "global_summary": "",
            "report_type": report_type.value,
            "start_time": start_time
        }
        
        try:
            # Execute the workflow
            final_state = await self.workflow.ainvoke(initial_state)
            
            # Convert internal dictionaries back to Pydantic models
            all_files = [self._dict_to_fileinfo(f) for f in final_state["all_files"]]
            file_summaries = [self._dict_to_filesummary(f) for f in final_state["file_summaries"]]
            
            # Generate folder structure analysis
            folder_structure = self._create_folder_structure(all_files)
            
            end_time = time.time()
            analysis_duration = end_time - start_time
            
            print(f"Analysis completed: {len(all_files)} files, {len(folder_structure)} folders, {analysis_duration:.1f}s")
            
            # Create the final structured response
            return RepositoryAnalysis(
                repository_path=repository_path,
                total_files=len(all_files),
                cobol_files_count=len([f for f in all_files if f.type == FileType.COBOL]),
                copybooks_count=len([f for f in all_files if f.type == FileType.COPYBOOK]),
                jcl_files_count=len([f for f in all_files if f.type == FileType.JCL]),
                sql_files_count=len([f for f in all_files if f.type == FileType.SQL]),
                folder_structure=folder_structure,
                file_summaries=file_summaries,
                global_summary=final_state["global_summary"],
                global_summary_markdown=self._generate_markdown(
                    final_state["global_summary"], 
                    all_files, 
                    folder_structure,
                    report_type
                ),
                report_type=report_type,
                analysis_duration=analysis_duration
            )
            
        except Exception as e:
            error_msg = f"Analysis failed: {str(e)}"
            print(error_msg)
            raise Exception(error_msg) from e
    
    def _scan_files(self, state: AnalysisState) -> AnalysisState:
        """Scan the repository and identify all analyzable files."""
        repo_path = Path(state["repository_path"])
        all_files = []
        
        # Recursively scan all files in the repository
        for file_path in repo_path.rglob('*'):
            if file_path.is_file():
                file_type = self._determine_file_type(file_path)
                
                if file_type:
                    try:
                        relative_path = file_path.relative_to(repo_path)
                        folder = str(relative_path.parent) if relative_path.parent != Path('.') else "root"
                    except ValueError:
                        folder = "external"
                    
                    file_dict = {
                        "path": str(file_path),
                        "name": file_path.name,
                        "size": file_path.stat().st_size,
                        "extension": file_path.suffix.lower(),
                        "type": file_type.value,
                        "folder": folder
                    }
                    all_files.append(file_dict)
        
        state["all_files"] = all_files
        
        # Concise file summary
        by_type = {}
        for f in all_files:
            by_type[f["type"]] = by_type.get(f["type"], 0) + 1
        
        type_summary = ", ".join([f"{t}:{c}" for t, c in by_type.items()])
        print(f"Found {len(all_files)} files ({type_summary})")
        
        return state
    
    def _analyze_files(self, state: AnalysisState) -> AnalysisState:
        """Analyze each individual file using AI."""
        file_summaries = []
        total_files = len(state["all_files"])
        
        print(f"Analyzing {total_files} files with AI...")
        
        for i, file_dict in enumerate(state["all_files"]):
            # Progress for large repositories only
            if total_files > 20 and (i + 1) % 10 == 0:
                print(f"Progress: {i+1}/{total_files}")
            
            try:
                summary = self._analyze_single_file_sync(file_dict)
                
                file_summaries.append({
                    "file_info": file_dict,
                    "summary": summary,
                    "analysis_success": True,
                    "error_message": None
                })
                
            except Exception as e:
                file_summaries.append({
                    "file_info": file_dict,
                    "summary": f"Analysis failed: {str(e)}",
                    "analysis_success": False,
                    "error_message": str(e)
                })
        
        successful_analyses = len([s for s in file_summaries if s["analysis_success"]])
        print(f"File analysis: {successful_analyses}/{total_files} successful")
        
        state["file_summaries"] = file_summaries
        return state
    
    def _generate_summary(self, state: AnalysisState) -> AnalysisState:
        """Generate a repository-level summary using AI."""
        print("Generating repository summary...")
        
        summary_info = self._prepare_summary_info(state["file_summaries"], state["all_files"])
        
        try:
            if self.prompt_manager:
                prompt = self.prompt_manager.get_summary_prompt(
                    report_type=state["report_type"],
                    summary_info=summary_info
                )
            else:
                prompt = self._get_fallback_summary_prompt(state["report_type"], summary_info)
            
            response = self.llm.invoke(prompt)
            state["global_summary"] = response.content
            
        except Exception as e:
            error_msg = f"Summary generation failed: {str(e)}"
            print(error_msg)
            state["global_summary"] = error_msg
        
        return state
    
    def _determine_file_type(self, file_path: Path) -> Optional[FileType]:
        """Determine the type of a file based on its extension."""
        ext = file_path.suffix.lower()
        
        if ext in settings.cobol_extensions:
            return FileType.COBOL
        elif ext in settings.copybook_extensions:
            return FileType.COPYBOOK
        elif ext in settings.jcl_extensions:
            return FileType.JCL
        elif ext in settings.sql_extensions:
            return FileType.SQL
        
        return None
    
    def _analyze_single_file_sync(self, file_dict: dict) -> str:
        """Analyze a single file using AI with appropriate prompts."""
        try:
            with open(file_dict["path"], 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
        except Exception as e:
            return f"Error reading file: {str(e)}"
        
        # Truncate large files to prevent token limit issues
        if len(content) > settings.max_content_length:
            content = content[:settings.max_content_length] + "\n... [content truncated for analysis]"
        
        try:
            if self.prompt_manager:
                prompt = self.prompt_manager.get_file_analysis_prompt(
                    file_type=file_dict["type"],
                    filename=file_dict["name"],
                    content=content
                )
            else:
                prompt = self._get_fallback_file_prompt(file_dict, content)
            
            response = self.llm.invoke(prompt)
            return response.content
            
        except Exception as e:
            return f"AI analysis error: {str(e)}"
    
    def _prepare_summary_info(self, file_summaries: List[dict], all_files: List[dict]) -> str:
        """Prepare comprehensive information for repository-level analysis."""
        info_parts = []
        
        # Statistical overview
        total_files = len(all_files)
        successful_analyses = len([s for s in file_summaries if s["analysis_success"]])
        
        info_parts.append("=== REPOSITORY STATISTICS ===")
        info_parts.append(f"Total files analyzed: {total_files}")
        info_parts.append(f"Successful analyses: {successful_analyses}")
        
        # File type breakdown
        by_type = {}
        for file_dict in all_files:
            file_type = file_dict["type"]
            by_type[file_type] = by_type.get(file_type, 0) + 1
        
        info_parts.append("\n=== FILE TYPE DISTRIBUTION ===")
        for file_type, count in by_type.items():
            info_parts.append(f"{file_type}: {count} files")
        
        # Folder organization
        by_folder = {}
        for file_dict in all_files:
            folder = file_dict["folder"]
            by_folder[folder] = by_folder.get(folder, 0) + 1
        
        info_parts.append("\n=== FOLDER ORGANIZATION ===")
        for folder, count in sorted(by_folder.items()):
            info_parts.append(f"{folder}: {count} files")
        
        # Individual file insights
        info_parts.append("\n=== INDIVIDUAL FILE ANALYSES ===")
        
        for summary in file_summaries:
            if summary["analysis_success"]:
                file_info = summary["file_info"]
                folder_info = f" (📁 {file_info['folder']})" if file_info['folder'] != "root" else ""
                
                info_parts.append(f"\n• {file_info['name']}{folder_info} ({file_info['type']}):")
                
                summary_text = summary["summary"]
                if len(summary_text) > 150:
                    summary_text = summary_text[:150] + "..."
                
                info_parts.append(f"  {summary_text}")
        
        return "\n".join(info_parts)
    
    def _get_fallback_file_prompt(self, file_dict: dict, content: str) -> str:
        """Fallback prompt when external prompt system is unavailable."""
        return f"""
        Analyze this {file_dict["type"]} file: {file_dict["name"]}
        
        Content:
        {content}

        Provide a concise analysis in English (maximum 150 words):
        - Primary purpose and functionality
        - Key technical observations
        - Business relevance (if applicable)
        
        Direct and professional response.
        """
    
    def _get_fallback_summary_prompt(self, report_type: str, summary_info: str) -> str:
        """Fallback prompt for repository summaries."""
        focus_guidance = {
            "business": "Focus on business processes, operational value, and strategic recommendations.",
            "technical": "Focus on technical architecture, code quality, and modernization opportunities.",
            "both": "Provide balanced coverage of business value and technical considerations."
        }
        
        guidance = focus_guidance.get(report_type, focus_guidance["both"])
        
        return f"""
        Analyze this COBOL repository comprehensively.
        
        Repository Information:
        {summary_info}
        
        {guidance}
        
        Provide a professional analysis in English covering:
        1. System purpose and architecture
        2. Key components and their functions  
        3. Technical and business observations
        4. Recommendations for improvement
        
        Maximum 600 words, structured and actionable.
        """
    
    def _dict_to_fileinfo(self, file_dict: dict) -> FileInfo:
        """Convert dictionary to FileInfo Pydantic model."""
        return FileInfo(
            path=file_dict["path"],
            name=file_dict["name"],
            size=file_dict["size"],
            extension=file_dict["extension"],
            type=FileType(file_dict["type"]),
            folder=file_dict["folder"]
        )
    
    def _dict_to_filesummary(self, summary_dict: dict) -> FileSummary:
        """Convert dictionary to FileSummary Pydantic model."""
        return FileSummary(
            file_info=self._dict_to_fileinfo(summary_dict["file_info"]),
            summary=summary_dict["summary"],
            analysis_success=summary_dict["analysis_success"],
            error_message=summary_dict.get("error_message")
        )
    
    def _create_folder_structure(self, all_files: List[FileInfo]) -> List[FolderStructure]:
        """Analyze the folder structure of the repository."""
        folder_stats = {}
        
        for file_info in all_files:
            folder = file_info.folder
            if folder not in folder_stats:
                folder_stats[folder] = {
                    "total": 0, "cobol": 0, "copybook": 0, 
                    "jcl": 0, "sql": 0, "other": 0
                }
            
            folder_stats[folder]["total"] += 1
            
            if file_info.type == FileType.COBOL:
                folder_stats[folder]["cobol"] += 1
            elif file_info.type == FileType.COPYBOOK:
                folder_stats[folder]["copybook"] += 1
            elif file_info.type == FileType.JCL:
                folder_stats[folder]["jcl"] += 1
            elif file_info.type == FileType.SQL:
                folder_stats[folder]["sql"] += 1
            else:
                folder_stats[folder]["other"] += 1
        
        folder_structure = []
        for folder, stats in folder_stats.items():
            description_parts = []
            if stats["cobol"] > 0:
                description_parts.append(f"{stats['cobol']} COBOL program(s)")
            if stats["copybook"] > 0:
                description_parts.append(f"{stats['copybook']} copybook(s)")
            if stats["jcl"] > 0:
                description_parts.append(f"{stats['jcl']} JCL file(s)")
            if stats["sql"] > 0:
                description_parts.append(f"{stats['sql']} SQL file(s)")
            
            if description_parts:
                description = f"Contains {', '.join(description_parts)}"
            else:
                description = f"Contains {stats['total']} files"
            
            folder_structure.append(FolderStructure(
                folder_path=folder,
                file_count=stats["total"],
                cobol_files=stats["cobol"],
                copybook_files=stats["copybook"],
                jcl_files=stats["jcl"],
                sql_files=stats["sql"],
                other_files=stats["other"],
                description=description
            ))
        
        return folder_structure
    
    def _generate_markdown(self, summary: str, all_files: List[FileInfo], 
                          folder_structure: List[FolderStructure], 
                          report_type: ReportType) -> str:
        """Generate a professional Markdown version of the analysis."""
        from datetime import datetime
        
        total_files = len(all_files)
        cobol_count = len([f for f in all_files if f.type == FileType.COBOL])
        jcl_count = len([f for f in all_files if f.type == FileType.JCL])
        copybook_count = len([f for f in all_files if f.type == FileType.COPYBOOK])
        sql_count = len([f for f in all_files if f.type == FileType.SQL])
        
        markdown = f"""# COBOL Repository Analysis Report

**Generated:** {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}  
**Report Type:** {report_type.value.title()}  
**Analysis Scope:** Comprehensive repository assessment

---

## 📊 Executive Summary

{summary}

---

## 📈 Repository Statistics

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total Files** | {total_files} | 100% |
| **COBOL Programs** | {cobol_count} | {(cobol_count/total_files*100 if total_files > 0 else 0.0):.1f}% |
| **JCL Files** | {jcl_count} | {(jcl_count/total_files*100 if total_files > 0 else 0.0):.1f}% |
| **Copybooks** | {copybook_count} | {(copybook_count/total_files*100 if total_files > 0 else 0.0):.1f}% |
| **SQL Files** | {sql_count} | {(sql_count/total_files*100 if total_files > 0 else 0.0):.1f}% |

---

## 📁 Repository Structure

| Folder | Files | COBOL | JCL | Copybooks | SQL | Description |
|--------|-------|-------|-----|-----------|-----|-------------|
"""
        
        for folder in sorted(folder_structure, key=lambda x: x.file_count, reverse=True):
            markdown += f"| `{folder.folder_path}` | {folder.file_count} | {folder.cobol_files} | {folder.jcl_files} | {folder.copybook_files} | {folder.sql_files} | {folder.description} |\n"
        
        markdown += f"""

---

*Report generated by COBOL Repository Analyzer v2.0*
"""
        
        return markdown