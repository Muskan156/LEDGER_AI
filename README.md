# LedgerAI 🧾

**AI-powered financial document processing platform.** Upload any bank statement PDF — LedgerAI automatically extracts, classifies, and organizes your transactions using a dual-pipeline AI engine (code-based extraction + LLM fallback).

---

## ✨ Features

- 📄 **Universal PDF Support** — Text-based, password-protected, scanned, and hybrid PDFs
- 🤖 **Dual-Pipeline AI Extraction** — LLM-generated code extractor runs alongside a direct Gemini LLM parser; the more accurate output wins
- 🏦 **Format Learning** — Automatically learns new bank statement formats and reuses them on future uploads (no re-learning needed)
- 🔒 **Secure** — Row-Level Security (RLS) via Supabase, JWT auth, AST-validated code sandbox
- 📊 **Review & Approve** — Side-by-side comparison of code vs LLM results before committing transactions
- 💾 **Supabase Storage** — PDFs stored in Supabase Storage (survives Render restarts)
- 🌐 **Deployed** — Frontend on Vercel, Backend on Render

---

## 🛠️ Tech Stack

### Backend
| Component | Technology |
|---|---|
| Framework | Python 3.x, FastAPI |
| AI Engine | Google Gemini (`google-genai`) |
| PDF Processing | `pdfplumber`, `PyPDF2`, `pypdf` |
| Database | Supabase (PostgreSQL) |
| Authentication | JWT, bcrypt |
| File Storage | Supabase Storage |
| Deployment | Render |

### Frontend
| Component | Technology |
|---|---|
| Framework | React 19 (Vite) |
| Routing | React Router 7 |
| Animations | Framer Motion |
| Icons | Lucide React |
| HTTP Client | Axios |
| Styling | Vanilla CSS |
| Deployment | Vercel |

---

## 🏗️ Architecture

```
User uploads PDF
       │
       ▼
┌─────────────────────────────────────────────────────────┐
│                    STEP 1: Upload                        │
│  PDF → Supabase Storage + DB record → background thread │
└─────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────┐
│               STEP 2: Text Extraction                    │
│  pdfplumber (3 strategies) → best page-by-page result   │
└─────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────┐
│            STEP 3: Format Identification                 │
│  Known format → ACTIVE fast path (code only)            │
│  New format   → Gemini classifies → generates extractor │
└─────────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────────┐
│          STEP 4: Dual Extraction (parallel)              │
│  ┌─────────────────┐    ┌─────────────────────────┐    │
│  │ CODE extractor  │    │  LLM direct parser      │    │
│  │ (fast, reusable)│    │  (Gemini, always works) │    │
│  └────────┬────────┘    └────────────┬────────────┘    │
│           └───────────┬──────────────┘                  │
└───────────────────────┼─────────────────────────────────┘
                        ▼
┌─────────────────────────────────────────────────────────┐
│           STEP 5: Validation & Decision                  │
│  Compare CODE vs LLM accuracy → pick winner             │
│  Winner stored in staging → await user review           │
└─────────────────────────────────────────────────────────┘
       │
       ▼
   User reviews & approves → transactions saved
```

---

## 📂 Project Structure

```
LedgerAI/
├── backend/                        # FastAPI Application
│   ├── api/
│   │   └── document_routes.py      # Upload, status, review, approve endpoints
│   ├── auth/
│   │   ├── routes.py               # Login / register
│   │   └── utils.py                # JWT verification
│   ├── db/
│   │   └── connection.py           # Supabase client
│   ├── repository/
│   │   ├── document_repo.py        # Document DB operations
│   │   └── statement_category_repo.py  # Format library DB operations
│   ├── services/
│   │   ├── processing_engine.py    # Main pipeline orchestrator
│   │   ├── pdf_service.py          # PDF text extraction (3-strategy)
│   │   ├── identifier_service.py   # Format classification & matching
│   │   ├── extraction_service.py   # LLM code generation & execution
│   │   ├── llm_parser.py           # Direct Gemini LLM extraction
│   │   ├── llm_retry.py            # Gemini retry wrapper
│   │   ├── validation_service.py   # Transaction validation
│   │   ├── code_sandbox.py         # AST-validated safe code execution
│   │   ├── post_process.py         # Post-processing utilities
│   │   └── prompts/                # Family-specific LLM prompts
│   ├── config.py                   # Environment variable loader
│   ├── main.py                     # FastAPI app entry point
│   └── requirements.txt
│
├── frontend/                       # React Application (Vite)
│   └── src/
│       ├── pages/
│       │   ├── AuthPage.jsx        # Login & Register
│       │   ├── Upload.jsx          # PDF upload & processing status
│       │   ├── Dashboard.jsx       # Document history
│       │   └── Review.jsx          # Transaction review & approval
│       ├── components/             # Reusable UI components
│       ├── api/                    # Axios API client
│       └── index.css               # Global design system
│
├── migration_script.sql            # Supabase schema setup
├── .env.example                    # Environment variable template
└── vercel.json                     # Vercel frontend config
```

---

## 🚀 Getting Started (Local Development)

### Prerequisites
- Python 3.9+
- Node.js 18+
- [Supabase](https://supabase.com) project (free tier works)
- [Google AI Studio](https://aistudio.google.com/) Gemini API Key

### 1. Clone the Repository
```bash
git clone https://github.com/your-username/LedgerAI.git
cd LedgerAI
```

### 2. Configure Environment Variables
```bash
cp .env.example .env
```

Edit `.env` with your credentials:
```env
# Supabase — found in Supabase Dashboard → Settings → API
SUPABASE_URL=https://your-project-ref.supabase.co
SUPABASE_ANON_KEY=your-anon-key
SUPABASE_SERVICE_ROLE_KEY=your-service-role-key

# Gemini AI
GEMINI_API_KEY=your-gemini-api-key
GEMINI_MODEL_NAME=models/gemini-2.0-flash
```

### 3. Set Up the Database
Run the migration script in your Supabase SQL Editor:
```
Supabase Dashboard → SQL Editor → paste migration_script.sql → Run
```

Also create a storage bucket named `financial_document_uploads` in:
```
Supabase Dashboard → Storage → New Bucket → financial_document_uploads (private)
```

### 4. Start the Backend
```bash
cd backend
python -m venv .venv

# Windows
.venv\Scripts\activate

# macOS/Linux
source .venv/bin/activate

pip install -r requirements.txt
python -m uvicorn main:app --reload --port 8000
```

Backend will be available at: `http://localhost:8000`

### 5. Start the Frontend
```bash
cd frontend
npm install
npm run dev
```

Frontend will be available at: `http://localhost:5173`

---

## ☁️ Deployment

### Backend — Render
1. Create a new **Web Service** on [Render](https://render.com)
2. Connect your GitHub repository
3. Set **Root Directory** to `backend`
4. Set **Start Command** to: `uvicorn main:app --host 0.0.0.0 --port $PORT`
5. Add all environment variables from `.env.example` under **Environment**

### Frontend — Vercel
1. Import your GitHub repository on [Vercel](https://vercel.com)
2. Set **Root Directory** to `frontend`
3. Add environment variable:
   ```
   VITE_API_URL=https://your-render-service.onrender.com
   ```

---

## 🔐 Security

- All PDF uploads are stored in private Supabase Storage (not publicly accessible)
- LLM-generated Python code runs in an AST-validated sandbox (`code_sandbox.py`) — no `exec`, `import`, `os`, or `subprocess` calls allowed
- Supabase Row-Level Security (RLS) ensures users can only access their own documents
- JWT tokens are validated on every authenticated request
- Passwords are hashed with bcrypt

---

## 📋 API Endpoints

| Method | Endpoint | Description |
|---|---|---|
| `POST` | `/auth/register` | Create new account |
| `POST` | `/auth/login` | Login, returns JWT |
| `POST` | `/documents/verify-type` | Detect PDF type before upload |
| `POST` | `/documents/upload` | Upload PDF, starts processing |
| `GET` | `/documents/status/{id}` | Poll processing status |
| `GET` | `/documents/{id}/review` | Get extracted transactions |
| `POST` | `/documents/{id}/approve` | Approve & save transactions |
| `DELETE` | `/documents/{id}` | Delete document & transactions |
| `GET` | `/documents/recent` | List recent documents |
| `GET` | `/documents/stats` | Document statistics |

---

## 📊 Document Processing Status Flow

```
UPLOADED → EXTRACTING_TEXT → IDENTIFYING_FORMAT → PARSING_TRANSACTIONS → AWAITING_REVIEW → APPROVE
                                                                                ↓
                                                                             FAILED
```

---

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Commit your changes: `git commit -m "Add my feature"`
4. Push to the branch: `git push origin feature/my-feature`
5. Open a Pull Request

---

## 📄 License

This project is licensed under the MIT License.