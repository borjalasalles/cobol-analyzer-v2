# ====================================================================
# Configuration Management
# ====================================================================
# This file now handles all configuration in a centralized, professional way.
# The key insight is that configuration should be external to code, allowing
# the same codebase to work in different environments (dev, test, prod).

import os
from typing import List

class Settings:
    """
    Centralized configuration management for the COBOL Analyzer.
    
    This class implements the "Configuration as Code" pattern, where all
    configurable parameters are loaded from environment variables. This
    approach provides several benefits:
    
    1. Environment Independence: Same code works in dev, test, and production
    2. Security: Sensitive data like API keys stay out of source code
    3. Flexibility: Operations teams can tune the system without code changes
    4. Scalability: Different instances can have different configurations
    """
    
    def __init__(self):
        # Load environment variables with sensible defaults
        from dotenv import load_dotenv
        load_dotenv()
        
        # API Configuration - Controls how the web service behaves
        self.anthropic_api_key = os.getenv("ANTHROPIC_API_KEY")
        self.api_host = os.getenv("API_HOST", "0.0.0.0")
        self.api_port = int(os.getenv("API_PORT", "8000"))
        self.debug = os.getenv("DEBUG", "false").lower() == "true"
        
        # Model Configuration - Controls AI behavior
        self.anthropic_model = os.getenv("ANTHROPIC_MODEL", "claude-sonnet-4-20250514")
        self.anthropic_temperature = float(os.getenv("ANTHROPIC_TEMPERATURE", "0.0"))
        self.anthropic_max_tokens = int(os.getenv("ANTHROPIC_MAX_TOKENS", "1000"))
        self.anthropic_timeout = int(os.getenv("ANTHROPIC_TIMEOUT", "300"))
        
        # Analysis Configuration - Controls processing behavior
        self.max_file_size_mb = int(os.getenv("MAX_FILE_SIZE_MB", "50"))
        self.max_content_length = int(os.getenv("MAX_CONTENT_LENGTH", "1500"))
        self.analysis_batch_size = int(os.getenv("ANALYSIS_BATCH_SIZE", "10"))
        
        # File Extension Mappings
        self.cobol_extensions = ['.cbl', '.cob', '.cobol', '.pgm']
        self.copybook_extensions = ['.cpy', '.copy', '.inc']
        self.jcl_extensions = ['.jcl', '.job']
        self.sql_extensions = ['.sql', '.sqlc']
        
        # Validate critical configuration
        self._validate_configuration()
    
    def _validate_configuration(self):
        """Validate that all required configuration is present and valid."""
        if not self.anthropic_api_key:
            raise ValueError("ANTHROPIC_API_KEY is required but not found in environment variables")
        
        if self.anthropic_temperature < 0 or self.anthropic_temperature > 1:
            raise ValueError("ANTHROPIC_TEMPERATURE must be between 0 and 1")
        
        if self.anthropic_max_tokens < 100:
            raise ValueError("ANTHROPIC_MAX_TOKENS must be at least 100")
        
        if self.max_content_length < 1000:
            raise ValueError("MAX_CONTENT_LENGTH must be at least 1000 characters")
    
    def get_model_config(self):
        """Return model configuration as a dictionary."""
        return {
            "model": self.anthropic_model,
            "temperature": self.anthropic_temperature,
            "max_tokens": self.anthropic_max_tokens,
            "timeout": self.anthropic_timeout
        }

# Create global settings instance with validation
try:
    settings = Settings()
    # Only show this once during import, not on reload
    if not hasattr(settings, '_initialized'):
        print(f"✅ Configuration loaded: {settings.anthropic_model}")
        settings._initialized = True
except Exception as e:
    print(f"❌ Configuration error: {e}")
    print("Please check your .env file and ensure all required variables are set")
    settings = None