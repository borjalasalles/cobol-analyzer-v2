import sys
import os

# Add the root directory to the Python path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import uvicorn
from cobol_analyzer.main import app
from cobol_analyzer.config import settings

def main():
    if not settings:
        print("Configuration error: Missing ANTHROPIC_API_KEY in .env file")
        sys.exit(1)
    
    # Clean, concise startup message
    print("COBOL Repository Analyzer")
    print("==============================")
    print(f"Server: http://{settings.api_host}:{settings.api_port}")
    print(f"Docs: http://{settings.api_host}:{settings.api_port}/docs")
    print(f"Health: http://{settings.api_host}:{settings.api_port}/health")
    print()
    
    uvicorn.run(
        "cobol_analyzer.main:app",
        host=settings.api_host,
        port=settings.api_port,
        reload=settings.debug
    )

if __name__ == "__main__":
    main()