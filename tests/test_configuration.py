import pytest
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from cobol_analyzer.config import settings

    def test_settings_loaded():
        """Test that settings are loaded correctly"""
        assert settings is not None
        assert hasattr(settings, 'anthropic_model')
        assert hasattr(settings, 'api_host')
        assert hasattr(settings, 'api_port')

    def test_file_extensions_configured():
        """Test that file extensions are properly configured"""
        assert settings.cobol_extensions is not None
        assert '.cbl' in settings.cobol_extensions
        assert '.cob' in settings.cobol_extensions
        
        assert settings.jcl_extensions is not None
        assert '.jcl' in settings.jcl_extensions

    def test_model_config():
        """Test that model configuration works"""
        config = settings.get_model_config()
        assert 'model' in config
        assert 'temperature' in config
        assert 'max_tokens' in config

except ImportError as e:
    print(f"Skipping configuration tests - missing dependencies: {e}")
    pytest.skip("Configuration not available", allow_module_level=True)