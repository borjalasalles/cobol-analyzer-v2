from fastapi import FastAPI, HTTPException, UploadFile, File
import aiofiles
import os
import tempfile
import zipfile
import logging
import traceback
import shutil
from pathlib import Path
from .analyzer import COBOLAnalyzer
from .models import AnalyzeRequest, RepositoryAnalysis, ReportType
from .config import settings

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Check configuration at startup
if not settings:
    raise Exception("Configuration could not be loaded. Check your .env file")

app = FastAPI(
    title="COBOL Repository Analyzer v2.0",
    description="Intelligent COBOL repository analyzer with external prompt management",
    version="1.0.0"
)

# Initialize analyzer
try:
    analyzer = COBOLAnalyzer()
    logger.info("Analyzer initialized successfully")
except Exception as e:
    logger.error(f"Error initializing analyzer: {e}")
    analyzer = None

@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "message": "COBOL Repository Analyzer API",
        "version": "1.0.0",
        "features": [
            "External prompt management",
            "Enhanced configuration system", 
            "Advanced AI model control",
            "Professional reporting"
        ],
        "docs": "/docs",
        "health": "/health"
    }

@app.post("/analyze-repository", response_model=RepositoryAnalysis)
async def analyze_repository_zip(
    file: UploadFile = File(...),
    report_type: ReportType = ReportType.BOTH
):
    """Analyze COBOL repository from ZIP file with enhanced error handling"""
    
    if not analyzer:
        logger.error("Analyzer not available")
        raise HTTPException(status_code=500, detail="Analyzer not available. Check configuration.")
    
    if not file.filename.endswith('.zip'):
        raise HTTPException(status_code=400, detail="Only ZIP files are accepted")
    
    temp_dir = None
    try:
        logger.info(f"=== STARTING ZIP ANALYSIS ===")
        logger.info(f"File: {file.filename}")
        logger.info(f"Report type: {report_type}")
        
        # Create temporary directory
        temp_dir = tempfile.mkdtemp()
        logger.info(f"Created temp directory: {temp_dir}")
        
        # Read file content
        logger.info("Reading ZIP content...")
        content = await file.read()
        file_size = len(content)
        logger.info(f"ZIP size: {file_size / (1024*1024):.1f} MB")
        
        # Check file size limit (100MB)
        max_size = 100 * 1024 * 1024
        if file_size > max_size:
            raise HTTPException(
                status_code=413, 
                detail=f"File too large: {file_size/(1024*1024):.1f}MB. Maximum: {max_size/(1024*1024):.1f}MB"
            )
        
        # Save ZIP file
        zip_path = os.path.join(temp_dir, file.filename)
        with open(zip_path, 'wb') as f:
            f.write(content)
        logger.info(f"ZIP saved to: {zip_path}")
        
        # Extract with validation
        extract_dir = os.path.join(temp_dir, "extracted")
        logger.info(f"Extracting to: {extract_dir}")
        
        try:
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                file_list = zip_ref.namelist()
                logger.info(f"ZIP contains {len(file_list)} files")
                
                # Limit number of files
                if len(file_list) > 5000:
                    logger.warning(f"Large repository: {len(file_list)} files")
                    raise HTTPException(
                        status_code=413, 
                        detail=f"Repository too large: {len(file_list)} files. Maximum: 5000"
                    )
                
                # Check total uncompressed size
                total_size = sum(info.file_size for info in zip_ref.filelist)
                logger.info(f"Total uncompressed size: {total_size / (1024*1024):.1f} MB")
                
                max_uncompressed = 500 * 1024 * 1024  # 500MB
                if total_size > max_uncompressed:
                    raise HTTPException(
                        status_code=413,
                        detail=f"Uncompressed content too large: {total_size/(1024*1024):.1f}MB. Maximum: {max_uncompressed/(1024*1024):.1f}MB"
                    )
                
                # Extract files
                zip_ref.extractall(extract_dir)
                logger.info("Extraction completed successfully")
                
        except zipfile.BadZipFile as e:
            logger.error(f"Invalid ZIP file: {e}")
            raise HTTPException(status_code=400, detail=f"Invalid ZIP file: {str(e)}")
        
        # Verify extracted content
        if not os.path.exists(extract_dir):
            raise HTTPException(status_code=500, detail="Extraction failed - directory not created")
        
        extracted_files = list(Path(extract_dir).rglob('*'))
        logger.info(f"Extracted {len(extracted_files)} total items")
        
        # Start analysis
        logger.info("=== STARTING REPOSITORY ANALYSIS ===")
        
        try:
            result = await analyzer.analyze_repository(extract_dir, report_type)
            logger.info(f"Analysis completed successfully. Files processed: {result.total_files}")
            return result
            
        except Exception as analysis_error:
            logger.error(f"Analysis failed: {analysis_error}")
            logger.error(f"Analysis error type: {type(analysis_error).__name__}")
            logger.error(f"Analysis traceback: {traceback.format_exc()}")
            
            # Provide detailed error information
            error_detail = {
                "stage": "repository_analysis",
                "error_type": type(analysis_error).__name__,
                "error_message": str(analysis_error),
                "repository_path": extract_dir,
                "files_found": len(extracted_files) if 'extracted_files' in locals() else 0
            }
            
            raise HTTPException(
                status_code=500, 
                detail=f"Repository analysis failed: {error_detail}"
            )
            
    except HTTPException:
        # Re-raise HTTP exceptions as-is
        raise
        
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        logger.error(f"Error type: {type(e).__name__}")
        logger.error(f"Full traceback: {traceback.format_exc()}")
        
        # Comprehensive error information
        error_details = {
            "stage": "file_processing",
            "error_type": type(e).__name__,
            "error_message": str(e),
            "file_name": file.filename if file else "unknown",
            "temp_dir": temp_dir if temp_dir else "not_created"
        }
        
        raise HTTPException(
            status_code=500, 
            detail=f"Processing failed: {error_details}"
        )
    
    finally:
        # Cleanup temporary files
        if temp_dir and os.path.exists(temp_dir):
            try:
                shutil.rmtree(temp_dir)
                logger.info(f"Cleaned up temp directory: {temp_dir}")
            except Exception as cleanup_error:
                logger.warning(f"Failed to cleanup temp directory: {cleanup_error}")

@app.post("/analyze-local", response_model=RepositoryAnalysis)
async def analyze_local_repository(request: AnalyzeRequest):
    """Analyze COBOL repository from local path"""
    
    if not analyzer:
        raise HTTPException(status_code=500, detail="Analyzer not available. Check configuration.")
    
    try:
        logger.info(f"=== STARTING LOCAL ANALYSIS ===")
        logger.info(f"Path: {request.repository_path}")
        logger.info(f"Report type: {request.report_type}")
        
        result = await analyzer.analyze_repository(request.repository_path, request.report_type)
        logger.info(f"Local analysis completed. Files processed: {result.total_files}")
        return result
        
    except FileNotFoundError as e:
        logger.error(f"File not found: {e}")
        raise HTTPException(status_code=404, detail=str(e))
        
    except Exception as e:
        logger.error(f"Local analysis failed: {e}")
        logger.error(f"Error type: {type(e).__name__}")
        logger.error(f"Traceback: {traceback.format_exc()}")
        
        raise HTTPException(
            status_code=500, 
            detail=f"Repository analysis failed: {type(e).__name__}: {str(e)}"
        )

@app.get("/health")
async def health_check():
    """Enhanced health check endpoint"""
    config_status = "OK" if settings else "ERROR"
    
    health_info = {
        "status": "healthy" if analyzer else "degraded",
        "service": "COBOL Repository Analyzer v2.0",
        "version": "2.0.0",
        "analyzer_ready": analyzer is not None,
        "configuration": config_status
    }
    
    if settings:
        health_info["model_config"] = settings.get_model_config()
    
    return health_info

@app.get("/prompts/status")
async def prompts_status():
    """Prompt management system status"""
    if not analyzer or not analyzer.prompt_manager:
        return {"status": "unavailable", "message": "Prompt manager not initialized"}
    
    try:
        available_prompts = analyzer.prompt_manager.list_available_prompts()
        return {
            "status": "active",
            "prompts_loaded": len(available_prompts),
            "available_prompts": list(available_prompts.keys())
        }
    except Exception as e:
        return {"status": "error", "message": str(e)}

# Add debug endpoint for troubleshooting
@app.get("/debug/info")
async def debug_info():
    """Debug information endpoint"""
    return {
        "settings_loaded": settings is not None,
        "analyzer_ready": analyzer is not None,
        "anthropic_model": settings.anthropic_model if settings else None,
        "api_key_configured": bool(settings.anthropic_api_key) if settings else False,
        "prompt_manager_ready": analyzer.prompt_manager is not None if analyzer else False
    }