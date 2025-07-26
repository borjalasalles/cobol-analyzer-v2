
import subprocess
import sys
import os

def run_all_tests():
    """Run all tests and show results"""
    
    test_files = [
        "test_api_endpoints.py",
        "test_file_detection.py", 
        "test_repository_analysis.py",
        "test_configuration.py",
        "test_repository_diagnostics.py"
    ]
    
    print("🧪 RUNNING ALL TESTS")
    print("=" * 50)
    
    total_passed = 0
    total_failed = 0
    
    for test_file in test_files:
        test_path = os.path.join("tests", test_file)
        
        if not os.path.exists(test_path):
            print(f"⚠️  {test_file}: File not found")
            continue
            
        print(f"\n🔍 Running {test_file}...")
        
        try:
            result = subprocess.run([
                sys.executable, "-m", "pytest", test_path, "-v"
            ], capture_output=True, text=True, cwd=os.path.dirname(os.path.dirname(__file__)))
            
            if result.returncode == 0:
                print(f"✅ {test_file}: PASSED")
                total_passed += 1
            else:
                print(f"❌ {test_file}: FAILED")
                print(f"   Error: {result.stderr[:200]}...")
                total_failed += 1
                
        except Exception as e:
            print(f"❌ {test_file}: ERROR - {e}")
            total_failed += 1
    
    print(f"\n" + "=" * 50)
    print(f"📊 TEST SUMMARY")
    print(f"✅ Passed: {total_passed}")
    print(f"❌ Failed: {total_failed}")
    print(f"📈 Success Rate: {total_passed/(total_passed+total_failed)*100:.1f}%" if (total_passed+total_failed) > 0 else "No tests run")

if __name__ == "__main__":
    run_all_tests()