import pytest
import zipfile
import tempfile
import sys
import os
import time
from pathlib import Path

class SimpleDiagnostic:
    """Diagnostic tool that works on Windows"""
    
    def analyze_zip(self, zip_path):
        """Analyze ZIP file contents"""
        result = {
            'total_files': 0,
            'cobol_files': 0,
            'jcl_files': 0,
            'other_files': 0
        }
        
        try:
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                file_list = zip_ref.namelist()
                result['total_files'] = len(file_list)
                
                for filename in file_list:
                    if filename.lower().endswith(('.cbl', '.cob', '.cobol')):
                        result['cobol_files'] += 1
                    elif filename.lower().endswith('.jcl'):
                        result['jcl_files'] += 1
                    else:
                        result['other_files'] += 1
                        
        except Exception as e:
            result['error'] = str(e)
            
        return result

def test_zip_diagnostic():
    """Test ZIP diagnostic with Windows-safe file handling"""
    
    # WINDOWS-SAFE METHOD: Use mkstemp instead of NamedTemporaryFile
    temp_fd, temp_zip_path = tempfile.mkstemp(suffix='.zip')
    
    try:
        # Close file descriptor immediately (important on Windows)
        os.close(temp_fd)
        
        # Create ZIP file
        with zipfile.ZipFile(temp_zip_path, 'w') as zf:
            zf.writestr('program1.cbl', 'IDENTIFICATION DIVISION.\nPROGRAM-ID. PROG1.')
            zf.writestr('job1.jcl', '//JOB1 JOB')
            zf.writestr('README.md', '# Test Repository')
        
        # Analyze ZIP
        diagnostic = SimpleDiagnostic()
        result = diagnostic.analyze_zip(temp_zip_path)
        
        # Verify results
        assert result['total_files'] == 3
        assert result['cobol_files'] == 1
        assert result['jcl_files'] == 1
        assert result['other_files'] == 1
        assert 'error' not in result
        
        print("PASS: ZIP diagnostic test (Windows-safe)")
        
    finally:
        # WINDOWS-SAFE CLEANUP: Try multiple times with delay
        if os.path.exists(temp_zip_path):
            for attempt in range(5):
                try:
                    os.unlink(temp_zip_path)
                    break
                except PermissionError:
                    if attempt < 4:
                        time.sleep(0.1)  # Wait 100ms
                    else:
                        print(f"WARNING: Could not delete temp file: {temp_zip_path}")

def test_diagnostic_logic():
    """Test diagnostic logic without files"""
    
    mock_files = [
        'src/program1.cbl',
        'src/program2.cob', 
        'jcl/compile.jcl',
        'docs/README.md'
    ]
    
    cobol_files = 0
    jcl_files = 0
    other_files = 0
    
    for filename in mock_files:
        if filename.lower().endswith(('.cbl', '.cob', '.cobol')):
            cobol_files += 1
        elif filename.lower().endswith('.jcl'):
            jcl_files += 1
        else:
            other_files += 1
    
    assert cobol_files == 2
    assert jcl_files == 1
    assert other_files == 1
    
    print("PASS: Diagnostic logic test")

if __name__ == "__main__":
    print("RUNNING DIAGNOSTIC TESTS")
    print("=" * 30)
    
    try:
        test_diagnostic_logic()
        test_zip_diagnostic()
        print("\nALL TESTS PASSED!")
    except Exception as e:
        print(f"\nTEST FAILED: {e}")
        import traceback
        traceback.print_exc()
