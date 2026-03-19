import { useEffect, useState, useRef } from "react";
import { motion } from "framer-motion";
import { FileUp, CheckCircle, Loader2, AlertCircle, Search, Cpu, List, Lock } from "lucide-react";
import AppLayout from "../components/Layout";
import API from "../api/api";
import { useNavigate } from "react-router-dom";

export default function UploadPage() {
    const navigate = useNavigate();

    // Auth check - ensure user is logged in
    useEffect(() => {
        const token = localStorage.getItem("token");
        if (!token) {
            navigate("/");
        }
    }, [navigate]);

    const [file, setFile] = useState(null);
    const [password, setPassword] = useState("");
    const [needsPassword, setNeedsPassword] = useState(false);
    const [pdfType, setPdfType] = useState(null);
    const [status, setStatus] = useState("IDLE");
    // IDLE → DETECTING → DETECTED → PASSWORD_REQUIRED → UPLOADING → PROCESSING → DONE → ERROR
    const [processingStatus, setProcessingStatus] = useState("");
    const [error, setError] = useState("");
    const [documentId, setDocumentId] = useState(null);
    const fileInputRef = useRef(null);

    const steps = [
        {
            label: "Upload & Detect",
            icon: Search,
            statuses: ["DETECTING", "DETECTED", "PASSWORD_REQUIRED", "UPLOADING", "PROCESSING",
                "EXTRACTING_TEXT", "IDENTIFYING_FORMAT", "PARSING_TRANSACTIONS", "AWAITING_REVIEW", "DONE"],
        },
        {
            label: "Text Extraction",
            icon: List,
            statuses: ["EXTRACTING_TEXT", "IDENTIFYING_FORMAT", "PARSING_TRANSACTIONS", "AWAITING_REVIEW", "DONE"],
        },
        {
            label: "Format Identification",
            icon: Search,
            statuses: ["IDENTIFYING_FORMAT", "PARSING_TRANSACTIONS", "AWAITING_REVIEW", "DONE"],
            subtext: "Checking if format exists in DB...",
        },
        {
            label: "Transaction Extraction",
            icon: Cpu,
            statuses: ["PARSING_TRANSACTIONS", "AWAITING_REVIEW", "DONE"],
            subtext: "Running extraction pipeline...",
        },
        {
            label: "Validation & Review",
            icon: CheckCircle,
            statuses: ["AWAITING_REVIEW", "DONE"],
            subtext: "Checking code accuracy...",
        },
    ];

    const getStepState = (step, idx) => {
        const currentStatus = processingStatus || status;
        const isIncluded = step.statuses.includes(currentStatus);
        // Check if this step is completed (subsequent step is active)
        const nextStep = steps[idx + 1];
        const nextActive = nextStep ? nextStep.statuses.includes(currentStatus) : false;

        if (currentStatus === "DONE" || currentStatus === "AWAITING_REVIEW") return "completed";
        if (nextActive) return "completed";
        if (isIncluded && !nextActive) return "active";
        return "pending";
    };

    // Descriptive substatus messages matching backend pipeline stages
    const getProcessingSubtext = () => {
        const currentStatus = processingStatus || status;
        switch (currentStatus) {
            case "EXTRACTING_TEXT":
                return "Extracting text from PDF pages...";
            case "IDENTIFYING_FORMAT":
                return "Checking if format exists in database...";
            case "PARSING_TRANSACTIONS":
                return "Running extraction pipeline (Code + LLM)...";
            case "AWAITING_REVIEW":
                return "Processing complete! Transactions ready for review.";
            default:
                return "";
        }
    };

    const onFileChange = async (e) => {
        const selectedFile = e.target.files[0];
        if (!selectedFile) return;
        if (!selectedFile.name.toLowerCase().endsWith('.pdf')) {
            setError("Only PDF files are supported.");
            return;
        }

        setFile(selectedFile);
        setError("");
        setPdfType(null);
        setNeedsPassword(false);
        setPassword("");
        setDocumentId(null);
        setProcessingStatus("");

        // Immediately detect PDF type
        setStatus("DETECTING");
        const formData = new FormData();
        formData.append("file", selectedFile);

        try {
            const res = await API.post("/documents/verify-type", formData);
            const type = res.data.pdf_type;
            setPdfType(type);

            if (type === "PASSWORD_TEXT_PDF") {
                setNeedsPassword(true);
                setStatus("PASSWORD_REQUIRED");
                setError("This PDF is password-protected. Please enter the password below.");
            } else if (type === "CORRUPTED_PDF") {
                setStatus("ERROR");
                setError("This file appears to be corrupted and cannot be processed.");
            } else if (type === "RESTRICTED_PDF") {
                setStatus("ERROR");
                setError("This PDF has restrictions that prevent text extraction.");
            } else {
                setStatus("DETECTED");
            }
        } catch (err) {
            setStatus("ERROR");
            setError(err.response?.data?.detail || "Failed to detect PDF type.");
        }
    };

    const handleUpload = async () => {
        if (!file) return;

        setStatus("UPLOADING");
        setError("");
        setProcessingStatus("EXTRACTING_TEXT");

        const formData = new FormData();
        formData.append("file", file);
        if (password) formData.append("password", password);

        try {
            const res = await API.post("/documents/upload", formData);
            const docId = res.data.document_id;
            setDocumentId(docId);
            setStatus("PROCESSING");

            // Poll for processing status
            const pollInterval = setInterval(async () => {
                try {
                    const statusRes = await API.get(`/documents/status/${docId}`);
                    const docStatus = statusRes.data.status;
                    setProcessingStatus(docStatus);

                    if (docStatus === "AWAITING_REVIEW" || docStatus === "APPROVE" || docStatus === "POSTED") {
                        clearInterval(pollInterval);
                        setStatus("DONE");
                        // Small delay then navigate to review
                        setTimeout(() => navigate(`/review?id=${docId}`), 1500);
                    } else if (docStatus === "FAILED") {
                        clearInterval(pollInterval);
                        setStatus("ERROR");
                        setError("Processing failed. The document could not be parsed.");
                    }
                } catch {
                    // Keep polling even if a single request fails
                }
            }, 2000);

            // Safety timeout — stop polling after 5 minutes
            setTimeout(() => {
                clearInterval(pollInterval);
            }, 300000);

        } catch (err) {
            setStatus("ERROR");
            setError(err.response?.data?.detail || "Upload failed. Please try again.");
        }
    };

    const isProcessing = ["UPLOADING", "PROCESSING"].includes(status);
    const showStepper = !["IDLE", "ERROR"].includes(status);
    const canUpload = file && !isProcessing && (status === "DETECTED" || (status === "PASSWORD_REQUIRED" && password));

    const typeLabel = {
        TEXT_PDF: "Text-based PDF",
        PASSWORD_TEXT_PDF: "Password-Protected PDF",
        SCANNED_PDF: "Scanned/Image PDF",
        IMAGE_CONVERTED_PDF: "Image-Converted PDF",
        HYBRID_PDF: "Hybrid PDF (Text + Images)",
        RESTRICTED_PDF: "Restricted PDF",
        CORRUPTED_PDF: "Corrupted PDF",
    };

    return (
        <AppLayout>
            <motion.div
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
            >
                <div style={{ marginBottom: '2rem' }}>
                    <h2 style={{ fontSize: '1.75rem', fontWeight: 800, color: '#1a1a2e' }}>Extract PDF</h2>
                </div>

                <div className="upload-page-card">
                    {/* Stepper */}
                    {showStepper && (
                        <div>
                            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', gap: '0.5rem', paddingBottom: '1.5rem', marginBottom: '0rem' }}>
                                {steps.map((step, i) => {
                                    const state = getStepState(step, i);
                                    return (
                                        <div key={i} style={{ display: 'flex', flexDirection: 'column', alignItems: 'center', flex: 1, position: 'relative' }}>
                                            <div style={{
                                                width: 32, height: 32, borderRadius: '50%',
                                                display: 'flex', alignItems: 'center', justifyContent: 'center',
                                                background: state === 'completed' ? '#27ae60' : state === 'active' ? '#483EA8' : '#f3f4f6',
                                                color: state === 'pending' ? '#9ca3af' : 'white',
                                                marginBottom: '0.5rem', zIndex: 1, transition: 'all 0.3s'
                                            }}>
                                                {state === 'completed' ? <CheckCircle size={16} /> :
                                                    state === 'active' ? <Loader2 size={16} className="spin-icon" /> :
                                                        <step.icon size={16} />}
                                            </div>
                                            <span style={{ fontSize: '0.65rem', fontWeight: 700, color: '#1a1a2e', textAlign: 'center', lineHeight: 1.2 }}>{step.label}</span>
                                            {i < steps.length - 1 && (
                                                <div style={{
                                                    position: 'absolute', top: 16, left: '50%', width: '100%', height: 2,
                                                    background: state === 'completed' ? '#27ae60' : '#e5e7eb', zIndex: 0
                                                }} />
                                            )}
                                        </div>
                                    );
                                })}
                            </div>
                            {/* Processing Status Banner */}
                            {isProcessing && (
                                <div style={{
                                    padding: '0.85rem 1rem',
                                    borderRadius: '12px',
                                    background: 'linear-gradient(135deg, #f0eeff 0%, #e8e4ff 100%)',
                                    border: '1px solid #d8d4f0',
                                    marginBottom: '1rem',
                                    display: 'flex',
                                    alignItems: 'center',
                                    gap: '0.75rem'
                                }}>
                                    <Loader2 size={16} className="spin-icon" style={{ color: '#483EA8' }} />
                                    <span style={{ fontSize: '0.85rem', fontWeight: 700, color: '#483EA8' }}>
                                        {getProcessingSubtext() || "Processing Document..."}
                                    </span>
                                </div>
                            )}
                        </div>
                    )}

                    {/* Dropzone Area */}
                    <div>
                        <div
                            className="dropzone"
                            onClick={() => !isProcessing && fileInputRef.current.click()}
                            style={{ opacity: isProcessing ? 0.6 : 1, cursor: isProcessing ? 'default' : 'pointer', minHeight: '260px' }}
                        >
                            <input
                                type="file"
                                hidden
                                ref={fileInputRef}
                                onChange={onFileChange}
                                accept=".pdf"
                            />
                            <FileUp size={48} className="dropzone-icon" />
                            <div className="dropzone-text" style={
                                file ? {
                                    maxWidth: '100%',
                                    overflow: 'hidden',
                                    textOverflow: 'ellipsis',
                                    whiteSpace: 'nowrap',
                                    padding: '0 1rem',
                                } : {}
                            }>
                                {file ? file.name : <>Drag or <span>upload file</span> here</>}
                            </div>
                            <div className="dropzone-hint">Supports PDF files only (Text-based, Password or Scanned)</div>
                        </div>

                        {/* PDF Type Badge */}
                        {pdfType && (
                            <div style={{
                                marginTop: '1.5rem', display: 'flex', alignItems: 'center', gap: '0.75rem',
                                padding: '1rem', borderRadius: '12px', background: '#f0eeff',
                                border: '1px solid #d8d4f0'
                            }}>
                                <Search size={16} color="#483EA8" />
                                <span style={{ fontSize: '0.85rem', fontWeight: 600, color: '#483EA8' }}>
                                    Detected: {typeLabel[pdfType] || pdfType}
                                </span>
                            </div>
                        )}

                        {/* Password Input */}
                        {needsPassword && (
                            <div style={{ marginTop: '2.5rem', textAlign: 'left' }}>
                                <label style={{ fontSize: '0.85rem', fontWeight: 700, color: '#111827', display: 'flex', alignItems: 'center', gap: '6px', marginBottom: '10px' }}>
                                    <Lock size={14} /> Document Password
                                </label>
                                <input
                                    type="password"
                                    className="auth-input"
                                    placeholder="Enter PDF password to unlock extraction..."
                                    value={password}
                                    onChange={(e) => setPassword(e.target.value)}
                                    style={{ background: '#f9f9fb', border: '1px solid #e5e7eb' }}
                                />
                            </div>
                        )}

                        {/* Error Message */}
                        {error && (
                            <div style={{
                                marginTop: '1.5rem', background: '#fdf0ef', border: '1px solid #fbdbd9',
                                padding: '1rem', borderRadius: '12px', display: 'flex', alignItems: 'center',
                                gap: '0.75rem', color: '#e74c3c', fontSize: '0.85rem', fontWeight: 600
                            }}>
                                <AlertCircle size={18} /> {error}
                            </div>
                        )}
                    </div>

                    {/* Final Action Button */}
                    <div style={{ marginTop: '1rem' }}>
                        <button
                            className="btn-submit"
                            disabled={!canUpload}
                            onClick={handleUpload}
                            style={{
                                width: '100%',
                                height: '56px',
                                fontSize: '1rem',
                                borderRadius: '12px',
                                boxShadow: '0 4px 12px rgba(72, 62, 168, 0.2)'
                            }}
                        >
                            {isProcessing ? (
                                <><Loader2 size={20} className="spin-icon" /> PROCESSING...</>
                            ) : status === "DONE" ? (
                                <><CheckCircle size={20} /> COMPLETED — REDIRECTING...</>
                            ) : (
                                "UPLOAD & START EXTRACTION"
                            )}
                        </button>
                    </div>
                </div>
            </motion.div>
        </AppLayout>
    );
}
