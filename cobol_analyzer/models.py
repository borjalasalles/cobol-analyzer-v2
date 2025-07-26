from pydantic import BaseModel, Field
from typing import List, Optional
from enum import Enum

class FileType(str, Enum):
    COBOL = "COBOL"
    COPYBOOK = "COPYBOOK"
    JCL = "JCL"
    SQL = "SQL"

class ReportType(str, Enum):
    BUSINESS = "business"
    TECHNICAL = "technical"
    BOTH = "both"

class FileInfo(BaseModel):
    path: str
    name: str
    size: int
    extension: str
    type: FileType
    folder: str = "root"

class FileSummary(BaseModel):
    file_info: FileInfo
    summary: str
    analysis_success: bool = True
    error_message: Optional[str] = None

class FolderStructure(BaseModel):
    folder_path: str
    file_count: int
    cobol_files: int
    copybook_files: int
    jcl_files: int
    sql_files: int
    other_files: int
    description: str

class RepositoryAnalysis(BaseModel):
    repository_path: str
    total_files: int
    cobol_files_count: int
    copybooks_count: int
    jcl_files_count: int
    sql_files_count: int
    folder_structure: List[FolderStructure]
    file_summaries: List[FileSummary]
    global_summary: str
    global_summary_markdown: str = ""
    report_type: ReportType = ReportType.BOTH
    analysis_duration: float

class AnalyzeRequest(BaseModel):
    repository_path: str
    report_type: ReportType = ReportType.BOTH
