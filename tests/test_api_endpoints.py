
import pytest
import sys
import os

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from fastapi.testclient import TestClient
    from cobol_analyzer.main import app
    
    client = TestClient(app)
    
    def test_root_endpoint():
        """Test that the API root works"""
        response = client.get("/")
        assert response.status_code == 200
        data = response.json()
        assert "message" in data
        assert "COBOL Repository Analyzer API" in data["message"]

    def test_health_endpoint():
        """Test that health check works"""
        response = client.get("/health")
        assert response.status_code == 200
        data = response.json()
        assert "status" in data
        assert "service" in data

    def test_debug_endpoint():
        """Test the new debug endpoint"""
        response = client.get("/debug/info")
        assert response.status_code == 200
        data = response.json()
        assert "settings_loaded" in data
        assert "analyzer_ready" in data

    def test_invalid_zip_rejection():
        """Test that non-ZIP files are rejected"""
        # Create a fake file
        fake_file = ("test.txt", b"not a zip file", "text/plain")
        
        response = client.post(
            "/analyze-repository",
            files={"file": fake_file}
        )
        assert response.status_code == 400
        assert "Only ZIP files are accepted" in response.json()["detail"]

    def test_missing_local_path():
        """Test that missing local paths return 404"""
        response = client.post("/analyze-local", json={
            "repository_path": "/this/path/does/not/exist"
        })
        assert response.status_code == 404

except ImportError as e:
    print(f"Skipping API tests - missing dependencies: {e}")
    pytest.skip("FastAPI or TestClient not available", allow_module_level=True)
