import pytest
import tempfile
import sys
import os
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from cobol_analyzer.analyzer import COBOLAnalyzer

    @pytest.fixture
    def sample_cobol_repo():
        """Create a temporary repository with sample COBOL files"""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create a simple COBOL program
            cobol_file = Path(temp_dir) / "hello.cbl"
            cobol_file.write_text("""
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
    DISPLAY 'Hello, World!'.
    STOP RUN.
""")
            
            # Create a copybook
            copybook = Path(temp_dir) / "employee.cpy"
            copybook.write_text("""
01 EMPLOYEE-RECORD.
   05 EMP-ID       PIC 9(5).
   05 EMP-NAME     PIC X(30).
   05 EMP-SALARY   PIC 9(8)V99.
""")
            
            # Create a JCL file
            jcl_file = Path(temp_dir) / "compile.jcl"
            jcl_file.write_text("""
//COMPILE JOB (12345),'COMPILE COBOL',CLASS=A
//STEP1 EXEC PGM=IGYCRCTL
//STEPLIB DD DSN=IGY630.SIGYCOMP,DISP=SHR
//SYSIN DD DSN=USER.COBOL.SOURCE(HELLO),DISP=SHR
//SYSOUT DD SYSOUT=*
""")
            
            yield temp_dir

    @pytest.mark.asyncio
    async def test_repository_analysis(sample_cobol_repo):
        """Test that repository analysis works with sample files"""
        analyzer = COBOLAnalyzer()
        
        result = await analyzer.analyze_repository(sample_cobol_repo)
        
        # Check basic results
        assert result.total_files >= 3  # Should find our 3 files
        assert result.cobol_files_count >= 1  # Should find the .cbl file
        assert result.copybooks_count >= 1   # Should find the .cpy file
        assert result.jcl_files_count >= 1   # Should find the .jcl file
        assert result.global_summary != ""   # Should have a summary
        assert result.analysis_duration > 0  # Should have taken some time

    @pytest.mark.asyncio 
    async def test_empty_repository():
        """Test behavior with empty repository"""
        with tempfile.TemporaryDirectory() as empty_dir:
            analyzer = COBOLAnalyzer()
            
            result = await analyzer.analyze_repository(empty_dir)
            
            assert result.total_files == 0
            assert result.cobol_files_count == 0
            assert result.global_summary != ""  # Should still have a summary

    def test_nonexistent_repository():
        """Test that nonexistent paths raise FileNotFoundError"""
        analyzer = COBOLAnalyzer()
        
        with pytest.raises(FileNotFoundError):
            import asyncio
            asyncio.run(analyzer.analyze_repository("/this/path/does/not/exist"))

except ImportError as e:
    print(f"Skipping repository analysis tests - missing dependencies: {e}")
    pytest.skip("COBOLAnalyzer not available", allow_module_level=True)