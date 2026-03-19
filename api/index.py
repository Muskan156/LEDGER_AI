import os
import sys

# Add root directory to sys.path so 'backend' and 'services' are importable
root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if root_dir not in sys.path:
    sys.path.append(root_dir)

from backend.main import app
