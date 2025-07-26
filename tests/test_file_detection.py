import pytest
import sys
import os
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from cobol_analyzer.analyzer import COBOLAnalyzer
    from cobol_analyzer.models import FileType

    def test_cobol_file_detection():
        """Test that COBOL files are detected correctly"""
        analyzer = COBOLAnalyzer()
        
        # Test different COBOL extensions
        assert analyzer._determine_file_type(Path("program.cbl")) == FileType.COBOL
        assert analyzer._determine_file_type(Path("program.cob")) == FileType.COBOL
        assert analyzer._determine_file_type(Path("program.cobol")) == FileType.COBOL

    def test_jcl_file_detection():
        """Test that JCL files are detected correctly"""
        analyzer = COBOLAnalyzer()
        
        assert analyzer._determine_file_type(Path("job.jcl")) == FileType.JCL
        assert analyzer._determine_file_type(Path("job.job")) == FileType.JCL

    def test_copybook_detection():
        """Test that copybooks are detected correctly"""
        analyzer = COBOLAnalyzer()
        
        assert analyzer._determine_file_type(Path("copybook.cpy")) == FileType.COPYBOOK
        assert analyzer._determine_file_type(Path("copybook.copy")) == FileType.COPYBOOK

    def test_unknown_file_detection():
        """Test that unknown files return None"""
        analyzer = COBOLAnalyzer()
        
        assert analyzer._determine_file_type(Path("readme.txt")) is None
        assert analyzer._determine_file_type(Path("image.png")) is None

except ImportError as e:
    print(f"Skipping file detection tests - missing dependencies: {e}")
    pytest.skip("COBOLAnalyzer not available", allow_module_level=True)
