**Professional AI-Powered Legacy Code Analysis System**

A sophisticated system for analyzing COBOL repositories using advanced language models and workflow orchestration. This project demonstrates modern software architecture principles applied to legacy code analysis, combining AI capabilities with robust engineering practices.


Attached is a lightweight repository belonging to IBM on Cobol tutorials as an example input, but you can actually download any remote repository from GitHub, or upload your own zip file or remote Cobol repository.
https://github.com/IBM/cobol-is-fun/tree/master
cobol-is-fun-master.zip

As well as .png captures and outputs obtained from a run already performed with Anthropic Haiku 3.5
-cobol_analysis_20250726_220230.json
-cobol_analysis_20250726_220230.md
-executive_summary_20250726_220230.txt

---

## 🎯 Project Overview

### What This System Does

The COBOL Repository Analyzer automatically examines COBOL codebases and generates comprehensive analysis reports. It combines artificial intelligence with structured workflow processing to provide both technical and business insights about legacy systems.

**Core Capabilities:**
- Automated scanning and classification of COBOL-related files
- Individual file analysis using specialized AI prompts
- Repository-level synthesis and strategic recommendations
- Multiple report types (business-focused, technical-focused, comprehensive)
- Professional output formats (JSON, Markdown, plain text)

### Target Use Cases

This system addresses critical needs in enterprise environments where COBOL systems remain business-critical:

**For Technical Teams:**
- Understanding inherited or undocumented COBOL systems
- Planning modernization and migration strategies
- Assessing technical debt and code quality
- Documenting legacy system architecture

**For Business Stakeholders:**
- Evaluating operational risks in legacy systems
- Planning digital transformation initiatives
- Understanding business process automation
- Making informed investment decisions about system modernization

**For Consulting Organizations:**
- Rapid assessment of client legacy systems
- Standardized analysis methodology across engagements
- Professional reporting for executive presentations
- Foundation for modernization proposals

---

## 🏗️ System Architecture

### High-Level Design Philosophy

The system implements a **layered architecture** with **separation of concerns**, making it maintainable, testable, and extensible. Each layer has distinct responsibilities and can be modified independently.

```
┌─────────────────────────────────────────────────┐
│                 Client Layer                    │
│            (User Interactions)                  │
├─────────────────────────────────────────────────┤
│                  API Layer                      │
│              (FastAPI Interface)                │
├─────────────────────────────────────────────────┤
│               Service Layer                     │
│         (Business Logic & AI Services)         │
├─────────────────────────────────────────────────┤
│               Core Layer                        │
│            (Workflow Orchestration)             │
├─────────────────────────────────────────────────┤
│            Configuration Layer                  │
│        (External Settings & Prompts)           │
└─────────────────────────────────────────────────┘
```

### Key Architectural Principles

**Workflow Orchestration with LangGraph:**
The system uses LangGraph to implement a **directed acyclic graph (DAG)** for processing workflows. This provides several advantages:
- **Clarity:** Processing steps are explicitly defined and visually understandable
- **Reliability:** Each step can handle errors independently without affecting others
- **Flexibility:** Easy to add, remove, or reorder processing steps
- **Monitoring:** Each step can be monitored and debugged separately

**External Configuration:**
All system behavior is controlled through environment variables and external files:
- **AI Model Selection:** Choose different Claude models without code changes
- **Processing Parameters:** Adjust analysis depth, timeout values, and batch sizes
- **Prompt Management:** Modify AI prompts without touching Python code

**Service-Oriented Design:**
Core functionality is organized into services that can be independently tested and reused:
- **Prompt Management Service:** Handles AI prompt loading and formatting
- **File Analysis Service:** Manages individual file processing
- **Configuration Service:** Centralizes system settings and validation

---

## 📁 Project Structure

### Directory Organization

```
cobol-analyzer/
├── cobol_analyzer/              # Main application package
│   ├── core/                    # Core business logic
│   │   ├── analyzer.py          # Main workflow orchestrator
│   │   └── models.py            # Data structure definitions
│   ├── services/                # Reusable business services
│   │   └── prompt_manager.py    # AI prompt management
│   ├── config.py                # Configuration management
│   └── main.py                  # FastAPI application
├── config/                      # External configuration
│   └── prompts/                 # AI prompt definitions
│       ├── file_analysis_prompts.yaml
│       ├── summary_prompts.yaml
│       └── validation_prompts.yaml
├── scripts/                     # Utility scripts
│   ├── run_server.py           # Server startup script
│   └── test_client.py          # Interactive test client
├── tools/                       # Development tools
│   └── visualize_workflow.py   # Workflow visualization
├── docs/                        # Documentation
│   └── README.md               # This file
└── tests/                       # Test suite
    ├── test_analyzer.py        # Core logic tests
    └── test_api.py             # API endpoint tests
```

### File Responsibilities

**Core Application Files:**

- `cobol_analyzer/analyzer.py`: The heart of the system. Implements the LangGraph workflow that orchestrates the entire analysis process from file scanning through final report generation.

- `cobol_analyzer/models.py`: Defines Pydantic data models that ensure type safety and data validation throughout the system. These models serve as contracts between different system components.

- `cobol_analyzer/config.py`: Centralizes all system configuration, reading from environment variables and providing validated settings to other components.

- `cobol_analyzer/main.py`: Implements the FastAPI web interface, providing RESTful endpoints for repository analysis.

**Service Layer Files:**

- `services/prompt_manager.py`: Manages AI prompts stored in external YAML files. This separation allows domain experts to optimize prompts without modifying code.

**Configuration Files:**

- `config/prompts/*.yaml`: Contains AI prompts organized by function. These files allow prompt optimization and A/B testing without code deployment.

**Utility Scripts:**

- `scripts/run_server.py`: Production-ready server startup with proper error handling and configuration validation.

- `scripts/test_client.py`: Interactive client for testing and demonstration purposes.

---

## 🔧 Technical Implementation

### Workflow Processing Engine

The system uses **LangGraph** as its workflow orchestration engine. LangGraph provides a declarative way to define processing pipelines as directed graphs, where each node represents a processing step and edges define the flow between steps.

**Workflow Stages:**

1. **File Scanning (`scan_files`)**: Recursively examines the repository structure, identifying and classifying files based on extensions and naming conventions.

2. **Individual Analysis (`analyze_files`)**: Processes each file independently using AI, applying specialized prompts based on file type (COBOL programs, copybooks, JCL, SQL).

3. **Global Synthesis (`generate_summary`)**: Combines individual file analyses into repository-level insights, tailored to the requested report type.

**State Management:**
The workflow uses a shared state object that flows between processing stages. This state contains all analysis data and is progressively enriched as it moves through the pipeline.

### AI Integration Architecture

The system integrates with **Anthropic's Claude** models through a configurable interface that supports different model variants and parameters.

**Model Configuration:**
```env
ANTHROPIC_MODEL=claude-3-5-haiku-20241022    # Primary model
ANTHROPIC_TEMPERATURE=0.2                   # Deterministic output
ANTHROPIC_MAX_TOKENS=1000                   # Response length limit
ANTHROPIC_TIMEOUT=300                       # Request timeout

```

**Prompt Management System:**
Rather than embedding prompts in code, the system loads them from external YAML files. This architecture provides several benefits:
- **Domain Expert Access:** Business analysts can optimize prompts without programming knowledge
- **Version Control:** Prompt changes are tracked separately from code changes  
- **A/B Testing:** Different prompt versions can be tested without code deployment
- **Consistency:** Prompts are centralized and reusable across different functions

### Data Models and Validation

The system uses **Pydantic** models for all data structures, providing automatic validation, type checking, and serialization. This approach ensures data integrity throughout the processing pipeline.

**Key Models:**
- `FileInfo`: Represents individual file metadata and classification
- `FileSummary`: Contains analysis results for individual files
- `RepositoryAnalysis`: Complete analysis results including statistics and summaries
- `FolderStructure`: Repository organization and component distribution

### Error Handling and Resilience

The system implements comprehensive error handling at multiple levels:

**File-Level Resilience:** If individual file analysis fails, the system continues processing other files and marks the failed analysis appropriately.

**Workflow-Level Recovery:** Each workflow stage includes error handling that allows the process to continue even if non-critical operations fail.

**Configuration Validation:** The system validates all configuration at startup, implementing "fail-fast" behavior to catch configuration errors early.

---

## 🚀 Installation and Setup

### Prerequisites

- **Python 3.9 or higher**: The system requires modern Python features and typing support
- **Anthropic API Access**: Valid API key for Claude model access
- **Minimum System Requirements**: 4GB RAM, sufficient for processing medium-sized repositories

### Installation Steps

1. **Clone and Setup Environment:**
```bash
git clone <repository-url>
cd cobol-analyzer
python -m venv .venv
source .venv/bin/activate  # Linux/Mac
# .venv\Scripts\activate   # Windows
```

2. **Install Dependencies:**
```bash
pip install -r requirements.txt
```

3. **Configure System:**
```bash
cp .env.example .env
# Edit .env with your configuration
```

4. **Verify Installation:**
```bash
python scripts/run_server.py
```

### Configuration Reference

**Required Environment Variables:**
```env
ANTHROPIC_API_KEY=your_api_key_here        # Required: Claude API access
```

**Optional Configuration:**
```env
API_HOST=0.0.0.0                          # Server binding address
API_PORT=8000                             # Server port
DEBUG=false                               # Enable debug logging
ANTHROPIC_MODEL=claude-3-sonnet-20240229  # AI model selection
ANTHROPIC_TEMPERATURE=0.0                 # AI response randomness
ANTHROPIC_MAX_TOKENS=4000                 # Maximum response length
```

---

## 📊 Usage Guide

### Basic Operation

**Starting the System:**
```bash
python scripts/run_server.py
```

**Using the Interactive Client:**
```bash
python scripts/test_client.py
```

**Direct API Access:**
```bash
curl -X POST "http://localhost:8000/analyze-local" \
     -H "Content-Type: application/json" \
     -d '{"repository_path": "/path/to/cobol/repo", "report_type": "comprehensive"}'
```

### Report Types

The system generates three distinct types of analysis reports:

**Business-Focused Reports:**
- Emphasis on operational value and business process automation
- Risk assessment and compliance considerations
- ROI analysis and modernization recommendations
- Executive-level language and strategic insights

**Technical-Focused Reports:**
- Code quality assessment and technical debt analysis
- Architecture patterns and design evaluation
- Modernization strategies and technology recommendations
- Developer-oriented language and implementation details

**Comprehensive Reports:**
- Balanced combination of business and technical perspectives
- Suitable for mixed audiences including both executives and technical teams
- Strategic recommendations with implementation guidance
- Complete coverage of all analysis dimensions

### Output Formats

**JSON Output:** Complete structured data suitable for integration with other systems or further processing.

**Markdown Reports:** Professional documentation format suitable for sharing with stakeholders, inclusion in technical documentation, or publishing on internal wikis.

**Executive Summaries:** Concise text format optimized for quick review and executive briefings.

---

## 🛠️ Development and Maintenance

### Code Organization Principles

The codebase follows established software engineering principles that make it maintainable and extensible:

**Single Responsibility Principle:** Each module and class has a clearly defined, single purpose. For example, the `PromptManager` class exclusively handles prompt loading and formatting, while the `COBOLAnalyzer` class focuses on workflow orchestration.

**Dependency Injection:** Components receive their dependencies rather than creating them internally. This makes the system more testable and flexible.

**Configuration Externalization:** All behavior-modifying parameters are externalized to environment variables or configuration files, allowing operation in different environments without code changes.

### Adding New Features

**Adding New File Types:**
1. Update file extension mappings in `config.py`
2. Create specialized prompts in appropriate YAML files
3. Add handling logic in the analyzer workflow
4. Update data models if necessary

**Adding New Report Types:**
1. Define new prompts in `summary_prompts.yaml`
2. Update the `ReportType` enum in `models.py`
3. Add handling logic in the summary generation workflow
4. Update API documentation

**Integrating New AI Models:**
1. Update configuration to support new model parameters
2. Modify the AI client initialization in `analyzer.py`
3. Test prompt compatibility with the new model
4. Update documentation with model-specific considerations

### Testing Strategy

The system includes comprehensive testing at multiple levels:

**Unit Tests:** Individual component testing with mocked dependencies
**Integration Tests:** End-to-end workflow testing with real AI model integration
**API Tests:** HTTP endpoint testing with various input scenarios
**Performance Tests:** Large repository processing and load testing

### Monitoring and Observability

**Logging Strategy:** The system implements structured logging at key processing stages, making it easy to trace request processing and identify performance bottlenecks.

**Metrics Collection:** Key metrics include processing time per file, AI model response times, error rates, and repository size statistics.

**Health Monitoring:** The system provides health check endpoints that verify all critical dependencies including AI model access and configuration validity.

---

## 🔮 Future Evolution Roadmap

### Agentic System Architecture

The current system provides an excellent foundation for evolution into a more sophisticated agentic system. The modular architecture and external configuration management make this transition straightforward.

**Multi-Agent Capabilities:**
- **Specialist Analysis Agents:** Different agents trained for specific COBOL analysis tasks (business logic analysis, data structure analysis, performance analysis)
- **Coordination Agent:** Master agent that orchestrates specialist agents based on repository characteristics
- **Quality Assurance Agent:** Dedicated agent for validating and improving analysis quality

**Open-Source Model Integration:**
The externalized configuration system makes it easy to integrate open-source models:
- **Cost Optimization:** Use smaller, fine-tuned models for routine analysis tasks
- **Specialized Models:** Custom models trained on specific COBOL patterns or business domains
- **Hybrid Approaches:** Combine multiple models for different aspects of analysis

### Scalability Enhancements

**Distributed Processing:**
- **Horizontal Scaling:** Process large repositories across multiple workers
- **Caching Layer:** Store analysis results for incremental repository analysis
- **Queue-Based Processing:** Handle multiple concurrent analysis requests

**Enterprise Integration:**
- **CI/CD Integration:** Automated analysis as part of development workflows
- **Enterprise Authentication:** Integration with corporate identity systems
- **Audit Trail:** Complete tracking of analysis history and changes

### Advanced Analysis Capabilities

**Dynamic Prompt Generation:**
- **Context-Aware Prompts:** Generate prompts based on repository characteristics
- **Adaptive Analysis:** Adjust analysis depth based on code complexity
- **Learning System:** Improve prompts based on analysis feedback

**Cross-Repository Analysis:**
- **Pattern Recognition:** Identify common patterns across multiple repositories
- **Best Practice Extraction:** Learn from high-quality codebases
- **Migration Strategy Generation:** Automated modernization planning

---

## 👥 Contributing and Maintenance

### Development Workflow

**Setting Up Development Environment:**
1. Fork the repository and create a feature branch
2. Install development dependencies: `pip install -r requirements-dev.txt`
3. Run tests to ensure baseline functionality: `pytest tests/`
4. Make changes following the established code organization principles

**Code Quality Standards:**
- **Type Hints:** All functions must include proper type annotations
- **Documentation:** All classes and functions require docstring documentation
- **Testing:** New features must include appropriate test coverage
- **Linting:** Code must pass flake8 and black formatting checks

**Pull Request Process:**
1. Ensure all tests pass and code coverage meets requirements
2. Update documentation for any API or behavior changes
3. Include performance impact assessment for core workflow changes
4. Provide clear description of changes and rationale

### Troubleshooting Common Issues

**Configuration Problems:**
- Verify `.env` file contains all required variables
- Check API key validity and quota limits
- Ensure proper file permissions for configuration files

**Performance Issues:**
- Monitor AI model response times and adjust timeout settings
- Consider repository size limits for very large codebases
- Review prompt complexity and token usage

**Analysis Quality Issues:**
- Review and optimize prompts in YAML files
- Adjust AI model temperature and response length parameters
- Implement validation prompts for quality assurance

---

## 📞 Support and Resources

### Documentation Resources

- **API Documentation:** Available at `/docs` endpoint when server is running
- **Workflow Visualization:** Use `tools/visualize_workflow.py` for visual system understanding
- **Configuration Reference:** See `.env.example` for all available settings

### Performance Considerations

**Repository Size Guidelines:**
- **Small Repositories (< 50 files):** Typically process in under 2 minutes
- **Medium Repositories (50-200 files):** Process in 5-15 minutes
- **Large Repositories (> 200 files):** May require 30+ minutes and increased timeout settings

**Cost Management:**
- Monitor AI model token usage for cost control
- Consider using less expensive models for initial analysis phases
- Implement caching for repeated analysis of unchanged repositories
