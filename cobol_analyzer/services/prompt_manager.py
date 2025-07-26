# ====================================================================
# External Prompt System
# ====================================================================
# This is the heart of our new prompt management system. The key insight
# is that prompts are a form of "business logic" that should be separated
# from code logic. This allows domain experts to optimize prompts without
# needing to understand Python.

import yaml
import os
from pathlib import Path
from typing import Dict, Any, Optional

class PromptManager:
    """  
    This class implements the "Separation of Concerns" principle by keeping
    AI prompts separate from Python code. This architecture provides:
    
    1. Maintainability: Prompts can be modified without touching code
    2. Collaboration: Domain experts can optimize prompts independently
    3. Version Control: Prompt changes are tracked separately from code changes
    4. Reusability: Same prompts can be used across different functions
    5. Testing: Prompts can be A/B tested without code deployment
    
    The prompts are stored in YAML files because YAML is human-readable,
    supports multi-line strings naturally, and can include metadata.
    """
    
    def __init__(self):
        self.config_dir = self._find_config_directory()
        self.prompts_cache = {}
        self._load_all_prompts()
        
        # Only show summary, not individual file loads
        if self.prompts_cache:
            print(f"External prompts loaded: {len(self.prompts_cache)} templates")
    
    def _find_config_directory(self) -> Path:
        """
        This method handles different execution contexts (running from different
        directories, deployment scenarios, etc.) by walking up the directory
        tree to find the config folder.
        """
        current_file = Path(__file__)
        
        # Walk up the directory tree to find the project root
        for parent in current_file.parents:
            config_dir = parent / "config" / "prompts"
            if config_dir.exists():
                return config_dir
        
        # If we can't find it in the standard location, try a fallback
        fallback_dir = Path.cwd() / "config" / "prompts"
        if fallback_dir.exists():
            return fallback_dir
        
        raise FileNotFoundError(
            f"Prompts directory not found. Expected at {config_dir} or {fallback_dir}. "
            "Please ensure the config/prompts directory exists in your project root."
        )
    
    def _load_all_prompts(self):
        """
        Load all prompt files into memory at startup.
        
        This caching approach provides several benefits:
        1. Performance: No file I/O during request processing
        2. Error Detection: Prompt file errors are caught at startup
        3. Consistency: All requests use the same prompt version
        """
        prompt_files = [
            "file_analysis_prompts.yaml",
            "summary_prompts.yaml",
            "validation_prompts.yaml"
        ]
        
        for filename in prompt_files:
            file_path = self.config_dir / filename
            if file_path.exists():
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        prompts = yaml.safe_load(f)
                        if prompts:
                            self.prompts_cache.update(prompts)
                except Exception as e:
                    print(f"Warning: Could not load {filename}: {e}")
    
    def get_file_analysis_prompt(self, file_type: str, filename: str, content: str) -> str:
        """
        Get the appropriate prompt for analyzing a specific file type.
        
        This method demonstrates the "Strategy Pattern" - different file types
        use different analysis strategies (prompts), but the interface remains
        the same.
        
        Args:
            file_type: Type of file (COBOL, COPYBOOK, JCL, SQL)
            filename: Name of the file being analyzed  
            content: Content of the file
            
        Returns:
            Formatted prompt ready for the LLM
        """
        prompt_key = f"{file_type.lower()}_analysis"
        
        if prompt_key in self.prompts_cache:
            prompt_template = self.prompts_cache[prompt_key]["prompt"]
        else:
            prompt_template = self._get_fallback_file_prompt()
        
        return prompt_template.format(
            filename=filename,
            content=content,
            file_type=file_type
        )
    
    def get_summary_prompt(self, report_type: str, summary_info: str) -> str:
        """
        Get the appropriate prompt for generating repository summaries.
        
        This method handles the different types of reports (business, technical,
        comprehensive) by selecting the appropriate prompt template.
        
        Args:
            report_type: Type of report (business, technical, both)
            summary_info: Compiled information about the repository
            
        Returns:
            Formatted prompt ready for the LLM
        """
        prompt_mapping = {
            "business": "business_focused",
            "technical": "technical_focused", 
            "both": "comprehensive"
        }
        
        prompt_key = prompt_mapping.get(report_type, "comprehensive")
        
        if prompt_key in self.prompts_cache:
            prompt_template = self.prompts_cache[prompt_key]["prompt"]
        else:
            prompt_template = self._get_fallback_summary_prompt()
        
        return prompt_template.format(summary_info=summary_info)
    
    def get_validation_prompt(self, validation_type: str, **kwargs) -> str:
        """
        Get prompts for validating AI outputs or intermediate results.
        
        This method supports quality assurance by providing prompts that can
        validate or improve the outputs from other AI operations.
        """
        prompt_key = f"{validation_type}_validation"
        
        if prompt_key in self.prompts_cache:
            prompt_template = self.prompts_cache[prompt_key]["prompt"]
            return prompt_template.format(**kwargs)
        else:
            return self._get_fallback_validation_prompt(**kwargs)
    
    def _get_fallback_file_prompt(self) -> str:
        """Fallback prompt when specific file type prompt is not available."""
        return """
        Analyze this {file_type} file: {filename}
        
        Content:
        {content}
        
        Provide a concise analysis in English (maximum 150 words):
        - Primary purpose
        - Key functionality
        - Important observations
        
        Direct and professional response.
        """
    
    def _get_fallback_summary_prompt(self) -> str:
        """Fallback prompt for repository summaries."""
        return """
        Analyze this COBOL repository comprehensively.
        
        Repository Information:
        {summary_info}
        
        Provide a professional analysis in English covering:
        1. System purpose and architecture
        2. Key components and their functions
        3. Technical observations
        4. Recommendations for improvement
        
        Maximum 600 words, structured and actionable.
        """
    
    def _get_fallback_validation_prompt(self, **kwargs) -> str:
        """Fallback prompt for validation tasks."""
        return """
        Review and validate the following analysis for accuracy and completeness.
        
        Content: {content}
        
        Provide feedback on:
        1. Accuracy of technical details
        2. Completeness of analysis
        3. Clarity of explanation
        4. Suggested improvements
        """
    
    def list_available_prompts(self) -> Dict[str, Any]:
        """Return a summary of all loaded prompts."""
        prompt_summary = {}
        for key, prompt_data in self.prompts_cache.items():
            prompt_summary[key] = {
                "context": prompt_data.get("context", "No context provided"),
                "length": len(prompt_data.get("prompt", "")),
                "has_template_vars": "{" in prompt_data.get("prompt", "")
            }
        return prompt_summary