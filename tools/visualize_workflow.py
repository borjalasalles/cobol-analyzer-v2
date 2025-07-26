# This utility script generates visual representations of the LangGraph workflow.
# It helps developers understand the processing flow and debug workflow issues.
# 
# Purpose: Create visual diagrams of the analysis workflow for:
# - Developer onboarding and training
# - System documentation and architecture reviews  
# - Debugging workflow execution paths
# - Stakeholder presentations and technical discussions

import sys
import os
from pathlib import Path

# Add project root to Python path for imports
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

def visualize_workflow():
    """
    Generate and display visual representation of the COBOL analyzer workflow.
    
    This function creates a visual diagram showing how data flows through
    the different processing stages. The visualization helps developers
    understand the system architecture and identify potential optimization
    points or debugging locations.
    """
    
    try:
        # Import the analyzer to access the workflow graph
        from cobol_analyzer.analyzer import COBOLAnalyzer
        
        print("🔍 Initializing COBOL Analyzer for workflow visualization...")
        
        # Create analyzer instance (this builds the workflow graph)
        analyzer = COBOLAnalyzer()
        
        # Access the compiled workflow graph
        workflow_graph = analyzer.workflow
        
        print("📊 Generating workflow visualization...")
        
        # Generate Mermaid diagram representation
        # Mermaid is a popular diagramming language that creates flowcharts
        try:
            # This creates a PNG image of the workflow
            mermaid_png = workflow_graph.get_graph().draw_mermaid_png()
            
            # Save the diagram to a file for reference
            output_path = project_root / "docs" / "workflow_diagram.png"
            output_path.parent.mkdir(exist_ok=True)
            
            with open(output_path, "wb") as f:
                f.write(mermaid_png)
            
            print(f"✅ Workflow diagram saved to: {output_path}")
            
            # Try to display in Jupyter environment if available
            try:
                from IPython.display import Image, display
                print("📱 Displaying workflow diagram...")
                display(Image(workflow_graph.get_graph().draw_mermaid_png()))
            except ImportError:
                print("ℹ️  IPython not available - diagram saved to file only")
                print(f"   View the diagram at: {output_path}")
            except Exception as display_error:
                print(f"⚠️  Display error: {display_error}")
                print(f"   Diagram still saved to: {output_path}")
                
        except Exception as mermaid_error:
            print(f"⚠️  Mermaid generation failed: {mermaid_error}")
            print("   Falling back to text representation...")
            _generate_text_diagram()
            
    except Exception as e:
        print(f"❌ Visualization failed: {e}")
        print("   This might be due to missing configuration or dependencies")
        print("   Ensure your .env file is properly configured")
        return False
    
    return True

def _generate_text_diagram():
    """
    Generate a simple text-based representation of the workflow.
    
    This fallback method provides a basic understanding of the workflow
    when graphical generation is not available. It's particularly useful
    in environments without graphics support or when dependencies are missing.
    """
    
    print("\n📋 WORKFLOW STRUCTURE (Text Representation):")
    print("=" * 60)
    
    workflow_steps = [
        ("START", "Repository path input", "Entry point for analysis"),
        ("scan_files", "File Discovery", "Recursively scan repository for COBOL-related files"),
        ("analyze_files", "Individual Analysis", "AI analysis of each file using specialized prompts"),
        ("generate_summary", "Global Synthesis", "Create repository-level insights and recommendations"),
        ("END", "Results Output", "Structured analysis results with multiple formats")
    ]
    
    for i, (step_name, step_title, step_description) in enumerate(workflow_steps):
        if i == 0:
            print(f"🟢 {step_name}: {step_title}")
        elif i == len(workflow_steps) - 1:
            print(f"🔴 {step_name}: {step_title}")
        else:
            print(f"🔵 {step_name}: {step_title}")
        
        print(f"   └─ {step_description}")
        
        if i < len(workflow_steps) - 1:
            print("   │")
            print("   ▼")
    
    print("\n📊 DATA FLOW SUMMARY:")
    print("   Input:  Repository path + Report type preference")
    print("   Stage 1: File scanning produces list of analyzable files")
    print("   Stage 2: Individual analysis produces file-level insights")  
    print("   Stage 3: Global analysis produces repository-level summary")
    print("   Output: Structured results (JSON) + Formatted report (Markdown)")

def generate_architecture_overview():
    """
    Create a high-level architecture overview for documentation purposes.
    
    This function generates diagrams and explanations that help stakeholders
    understand the system components and their relationships. It's particularly
    valuable for technical reviews and future development planning.
    """
    
    print("\n🏗️  SYSTEM ARCHITECTURE OVERVIEW:")
    print("=" * 60)
    
    components = [
        ("Configuration Layer", [
            "Environment variables (.env)",
            "Settings management (config.py)", 
            "External prompt definitions (YAML files)"
        ]),
        ("Core Processing Layer", [
            "LangGraph workflow orchestration",
            "AI model integration (Anthropic Claude)",
            "File analysis and synthesis logic"
        ]),
        ("Service Layer", [
            "Prompt management system",
            "File type detection and handling",
            "Result formatting and validation"
        ]),
        ("API Layer", [
            "FastAPI web interface",
            "Request/response handling",
            "Authentication and error management"
        ]),
        ("Client Layer", [
            "Interactive command-line client",
            "Batch processing capabilities", 
            "Multiple output format generation"
        ])
    ]
    
    for layer_name, layer_components in components:
        print(f"\n📦 {layer_name}:")
        for component in layer_components:
            print(f"   • {component}")
    
    print(f"\n🔄 PROCESSING FLOW:")
    print("   API Request → Configuration Loading → Workflow Execution")
    print("   → File Scanning → Individual Analysis → Global Synthesis")
    print("   → Result Formatting → Response Delivery")

def main():
    """
    Main function to run workflow visualization and architecture overview.
    
    This function provides a complete view of the system structure and
    processing flow, making it an excellent tool for developer onboarding
    and system documentation.
    """
    
    print("🎯 COBOL ANALYZER - WORKFLOW VISUALIZATION TOOL")
    print("=" * 60)
    print("This tool helps understand the system architecture and processing flow")
    print()
    
    # Generate workflow visualization
    success = visualize_workflow()
    
    # Always show architecture overview
    generate_architecture_overview()
    
    if success:
        print(f"\n✅ Visualization completed successfully!")
        print("   Use this information for:")
        print("   • Developer onboarding and training")
        print("   • System architecture documentation") 
        print("   • Debugging and optimization planning")
        print("   • Stakeholder technical presentations")
    else:
        print(f"\n⚠️  Visualization partially completed")
        print("   Text representations are available for system understanding")

if __name__ == "__main__":
    main()