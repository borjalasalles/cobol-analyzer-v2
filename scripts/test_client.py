# ====================================================================
# Test Client
# ====================================================================
# This is the user-facing test client, now fully internationalized to English.
# The improvements focus on better user experience and clearer reporting.

import sys
import os

# Add the project root to the Python path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import requests
import json
from pathlib import Path
from typing import Optional, Dict, Any

class COBOLAnalyzerClient:
    """
    Professional client for interacting with the COBOL Analyzer API.
    
    This client provides a user-friendly interface for repository analysis
    with comprehensive error handling and result presentation.
    """
    
    def __init__(self, base_url: str = "http://localhost:8000"):
        self.base_url = base_url
    
    def test_connection(self) -> bool:
        """Test connection to the analyzer server."""
        try:
            response = requests.get(f"{self.base_url}/health", timeout=10)
            response.raise_for_status()
            result = response.json()
            
            print("✅ Connection successful to analyzer server")
            print(f"   Status: {result.get('status', 'unknown')}")
            print(f"   Service: {result.get('service', 'unknown')}")
            print(f"   Version: {result.get('version', 'unknown')}")
            print(f"   Analyzer ready: {result.get('analyzer_ready', 'unknown')}")
            
            if 'model_config' in result:
                model_config = result['model_config']
                print(f"   AI Model: {model_config.get('model', 'unknown')}")
                print(f"   Temperature: {model_config.get('temperature', 'unknown')}")
            
            return result.get('status') == 'healthy'
            
        except requests.exceptions.RequestException as e:
            print(f"❌ Connection failed: {e}")
            return False
        except Exception as e:
            print(f"❌ Unexpected error: {e}")
            return False
    
    def analyze_local_repository(self, repository_path: str, report_type: str = "both") -> Optional[Dict[Any, Any]]:
        """Analyze a local repository."""
        url = f"{self.base_url}/analyze-local"
        payload = {
            "repository_path": repository_path,
            "report_type": report_type
        }
        
        try:
            print(f"🔍 Analyzing repository: {repository_path}")
            print(f"📊 Report type: {report_type}")
            print("⏳ This may take a few minutes for large repositories...")
            
            response = requests.post(url, json=payload, timeout=600)
            response.raise_for_status()
            
            result = response.json()
            self._display_results(result)
            self._save_results(result)
            return result
            
        except requests.exceptions.Timeout:
            print("❌ Analysis timeout: The repository may be too large or complex")
            print("   Try analyzing a smaller subset or contact support")
            return None
        except requests.exceptions.RequestException as e:
            print(f"❌ Request failed: {e}")
            if hasattr(e, 'response') and e.response is not None:
                try:
                    error_detail = e.response.json()
                    print(f"   Server error: {error_detail.get('detail', 'Unknown error')}")
                except:
                    print(f"   HTTP {e.response.status_code}: {e.response.text}")
            return None
        except Exception as e:
            print(f"❌ Unexpected error: {e}")
            return None
    
    def analyze_zip_repository(self, zip_path: str, report_type: str = "both") -> Optional[Dict[Any, Any]]:
        """Analyze a repository from a ZIP file."""
        url = f"{self.base_url}/analyze-repository"
        
        try:
            print(f"📦 Analyzing ZIP file: {zip_path}")
            print(f"📊 Report type: {report_type}")
            print("⏳ Uploading and analyzing... this may take several minutes...")
            
            with open(zip_path, 'rb') as f:
                files = {'file': (Path(zip_path).name, f, 'application/zip')}
                params = {'report_type': report_type}
                response = requests.post(url, files=files, params=params, timeout=600)
            
            response.raise_for_status()
            result = response.json()
            self._display_results(result)
            self._save_results(result)
            return result
            
        except FileNotFoundError:
            print(f"❌ File not found: {zip_path}")
            print("   Please check the file path and try again")
            return None
        except requests.exceptions.RequestException as e:
            print(f"❌ Upload/analysis failed: {e}")
            return None
        except Exception as e:
            print(f"❌ Unexpected error: {e}")
            return None
    
    def _display_results(self, result: Dict[Any, Any]) -> None:
        """Display analysis results in a user-friendly format."""
        print("\n" + "="*80)
        print("🎉 ANALYSIS COMPLETED SUCCESSFULLY!")
        print("="*80)
        
        # Repository Overview
        print("\n📊 REPOSITORY OVERVIEW:")
        print(f"   📍 Location: {result['repository_path']}")
        print(f"   📁 Total files: {result['total_files']}")
        print(f"   ⏱️  Analysis duration: {result['analysis_duration']:.2f} seconds")
        print(f"   📋 Report type: {result.get('report_type', 'unknown').title()}")
        
        # File Type Breakdown
        print(f"\n📈 FILE COMPOSITION:")
        if result['cobol_files_count'] > 0:
            print(f"   🔵 COBOL Programs: {result['cobol_files_count']}")
        if result['jcl_files_count'] > 0:
            print(f"   🟡 JCL Files: {result['jcl_files_count']}")
        if result['copybooks_count'] > 0:
            print(f"   🟢 Copybooks: {result['copybooks_count']}")
        if result['sql_files_count'] > 0:
            print(f"   🔴 SQL Files: {result['sql_files_count']}")
        
        # Folder Structure (if available)
        if 'folder_structure' in result and result['folder_structure']:
            print(f"\n📁 FOLDER STRUCTURE:")
            for folder in result['folder_structure']:
                print(f"   📂 {folder['folder_path']} ({folder['file_count']} files)")
                print(f"      {folder['description']}")
        
        # Main Analysis Results
        print(f"\n📋 ANALYSIS SUMMARY:")
        print("-" * 60)
        summary_lines = result['global_summary'].split('\n')
        for line in summary_lines:
            if line.strip():
                print(f"   {line}")
        
        print("\n" + "="*80)
    
    def _save_results(self, result: Dict[Any, Any]) -> None:
        """Save analysis results in multiple formats."""
        timestamp = self._get_timestamp()
        
        # Save complete JSON results
        json_filename = f'cobol_analysis_{timestamp}.json'
        try:
            with open(json_filename, 'w', encoding='utf-8') as f:
                json.dump(result, f, ensure_ascii=False, indent=2)
            print(f"💾 Complete results saved: {json_filename}")
        except Exception as e:
            print(f"⚠️  Could not save JSON: {e}")
        
        # Save Markdown report (if available)
        if 'global_summary_markdown' in result and result['global_summary_markdown']:
            md_filename = f'cobol_analysis_{timestamp}.md'
            try:
                with open(md_filename, 'w', encoding='utf-8') as f:
                    f.write(result['global_summary_markdown'])
                print(f"📄 Markdown report saved: {md_filename}")
            except Exception as e:
                print(f"⚠️  Could not save Markdown: {e}")
        
        # Save executive summary
        summary_filename = f'executive_summary_{timestamp}.txt'
        try:
            with open(summary_filename, 'w', encoding='utf-8') as f:
                f.write("COBOL REPOSITORY ANALYSIS - EXECUTIVE SUMMARY\n")
                f.write("=" * 50 + "\n\n")
                f.write(f"Repository: {result['repository_path']}\n")
                f.write(f"Analysis Date: {timestamp}\n")
                f.write(f"Report Type: {result.get('report_type', 'unknown').title()}\n\n")
                f.write("STATISTICS:\n")
                f.write(f"- Total files: {result['total_files']}\n")
                f.write(f"- COBOL programs: {result['cobol_files_count']}\n")
                f.write(f"- JCL files: {result['jcl_files_count']}\n")
                f.write(f"- Copybooks: {result['copybooks_count']}\n")
                f.write(f"- SQL files: {result['sql_files_count']}\n")
                f.write(f"- Analysis duration: {result['analysis_duration']:.2f}s\n\n")
                f.write("ANALYSIS:\n")
                f.write(result['global_summary'])
            print(f"📋 Executive summary saved: {summary_filename}")
        except Exception as e:
            print(f"⚠️  Could not save summary: {e}")
    
    def _get_timestamp(self) -> str:
        """Generate timestamp for file naming."""
        from datetime import datetime
        return datetime.now().strftime("%Y%m%d_%H%M%S")

def display_welcome_message():
    """Display welcome message and instructions."""
    print("🔍 COBOL REPOSITORY ANALYZER")
    print("=" * 50)
    print("Professional analysis of COBOL codebases using AI")

def get_analysis_options():
    """Get analysis options from user."""
    print("📋 ANALYSIS OPTIONS:")
    print("1. Analyze local repository")
    print("2. Analyze ZIP file")
    
    while True:
        choice = input("\nSelect an option (1 or 2): ").strip()
        if choice in ["1", "2"]:
            return choice
        print("❌ Please enter 1 or 2")

def get_report_type():
    """Get report type preference from user."""
    print("\n📊 REPORT TYPE:")
    print("1. Business Focus    - Processes, value, operational impact")
    print("2. Technical Focus   - Architecture, code quality, modernization")
    print("3. Comprehensive     - Combined business and technical (recommended)")
    
    report_mapping = {
        "1": ("business", "Business-focused"),
        "2": ("technical", "Technical-focused"),
        "3": ("both", "Comprehensive")
    }
    
    while True:
        choice = input("\nSelect report type (1/2/3): ").strip()
        if choice in report_mapping:
            report_type, description = report_mapping[choice]
            print(f"✅ Selected: {description} analysis")
            return report_type
        print("❌ Please enter 1, 2, or 3")

def get_repository_path():
    """Get repository path from user with validation."""
    while True:
        path = input("\nEnter the repository path: ").strip().strip('"\'')
        
        if not path:
            print("❌ Please enter a valid path")
            continue
        
        if path.lower().endswith('.zip'):
            if Path(path).exists():
                return path
            else:
                print(f"❌ ZIP file not found: {path}")
                continue
        else:
            if Path(path).exists() and Path(path).is_dir():
                return path
            else:
                print(f"❌ Directory not found: {path}")
                continue

def main():
    """Main function for the COBOL Analyzer test client."""
    display_welcome_message()
    
    # Initialize client and test connection
    client = COBOLAnalyzerClient()
    
    print("🔗 Testing connection to analyzer service...")
    if not client.test_connection():
        print("\n❌ Cannot connect to the analyzer service.")
        print("   Please ensure the server is running with: python scripts/run_server.py")
        return
    
    try:
        # Get user preferences
        analysis_option = get_analysis_options()
        report_type = get_report_type()
        
        print(f"\n📁 PATH SELECTION:")
        if analysis_option == "1":
            print("Enter the path to your COBOL repository directory:")
        else:
            print("Enter the path to your ZIP file containing the repository:")
        
        repository_path = get_repository_path()
        
        # Perform analysis
        print(f"\n🚀 STARTING ANALYSIS...")
        print("    This process will:")
        print("    • Scan all files in the repository")
        print("    • Analyze each file with enhanced AI prompts")
        print("    • Generate comprehensive summary")
        print("    • Create multiple output formats")
        print()
        
        if analysis_option == "1":
            result = client.analyze_local_repository(repository_path, report_type)
        else:
            result = client.analyze_zip_repository(repository_path, report_type)
        
        if result:
            print("\n🎯 WHAT'S NEXT?")
            print("• Review the generated files for detailed insights")
            print("• Share the Markdown report with stakeholders")
            print("• Use the JSON data for further processing")
            print("• Consider the recommendations for next steps")
        else:
            print("\n❌ Analysis failed. Please check the error messages above.")
            
    except KeyboardInterrupt:
        print("\n\n⏹️  Analysis cancelled by user")
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        print("Please report this issue if it persists")

if __name__ == "__main__":
    main()
