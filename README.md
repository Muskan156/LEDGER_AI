# LedgerAI 🚀

## 🛠️ Tech Stack

### **Backend**
- **Framework**: Python 3.x, FastAPI
- **AI Engine**: Google Gemini (via `google-genai`)
- **PDF Processing**: `pdfplumber`, `pypdf`
- **Database**: MySQL with Connection Pooling
- **Authentication**: JWT, bcrypt

### **Frontend**
- **Library**: React 19 (Vite)
- **Routing**: React Router 7
- **Animations**: Framer Motion
- **Icons**: Lucide React
- **Styling**: Vanilla CSS (Custom modern design system)

---

## 🚀 Getting Started

### **Prerequisites**
- Python 3.9+
- Node.js 18+
- MySQL Server
- Google Gemini API Key

### **1. Clone & Environment Setup**
```bash
# Register for a Gemini API Key at https://aistudio.google.com/
```

Create a `.env` file in the root directory:
```env
DB_HOST=localhost
DB_USER=your_user
DB_PASSWORD=your_password
DB_NAME=ledger_db
GEMINI_API_KEY=your_gemini_key
GEMINI_MODEL_NAME=models/gemini-2.0-flash
JWT_SECRET=your_secret_key
```

### **2. Database Setup**
Import the schema into your MySQL instance:
```bash
mysql -u your_user -p ledger_db < db/ledger_db.sql
```

### **3. Backend Installation**
```bash
python -m venv .venv
source .venv/bin/activate  # or `.venv\Scripts\activate` on Windows
pip install -r requirements.txt
python -m uvicorn backend.main:app --reload --port 8000
```

### **4. Frontend Installation**
```bash
cd frontend
npm install
npm run dev
```

---

## 📂 Project Structure

```text
├── backend/                # FastAPI Application
│   ├── api/                # Core API routes
│   ├── auth/               # Authentication logic & routes
│   ├── db/                 # FastAPI specific DB patterns
│   └── main.py             # Entry point
├── frontend/               # React Application
│   ├── src/
│   │   ├── components/     # Reusable UI components
│   │   ├── pages/          # Full page views (Upload, Dashboard, etc.)
│   │   └── api/            # Axios API client
├── services/               # Core Business Logic
│   ├── extraction_service.py # AI extraction orchestration
│   ├── llm_parser.py       # LLM interaction layer
│   └── code_sandbox.py     # Safe code execution
├── repository/             # Data Access Layer
└── db/                     # Global DB configuration & SQL schema
```