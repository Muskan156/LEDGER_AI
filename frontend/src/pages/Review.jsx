import { useState, useEffect } from "react";
import { useSearchParams, useNavigate } from "react-router-dom";
import { motion } from "framer-motion";
import { Check, Code, FileSearch, Building2, Cpu, Loader2, ChevronLeft, CheckCircle, Download, CreditCard, AlertCircle, Link } from "lucide-react";
import AppLayout from "../components/Layout";
import API from "../api/api";

export default function ReviewPage() {
    const [searchParams] = useSearchParams();
    const documentId = searchParams.get("id");
    const navigate = useNavigate();

    const [data, setData] = useState(null);
    const [isLoading, setIsLoading] = useState(true);
    const [error, setError] = useState("");
    const [isApproved, setIsApproved] = useState(false);
    const [isApproving, setIsApproving] = useState(false);
    const [accountInfo, setAccountInfo] = useState([]); // detected accounts from pipeline
    // Per-account action state — keyed by array index
    // status: null | "saving" | "saved" | "skipped"
    const [accountStatus, setAccountStatus] = useState({});

    const setAcctStatus = (idx, status) =>
        setAccountStatus(prev => ({ ...prev, [idx]: status }));

    useEffect(() => {
        if (!documentId) {
            setError("No document ID provided.");
            setIsLoading(false);
            return;
        }

        const fetchReviewData = async () => {
            try {
                const res = await API.get(`/documents/${documentId}/review`);
                setData(res.data);
                // Populate account banner from review response
                if (res.data.account_info && res.data.account_info.length > 0) {
                    setAccountInfo(res.data.account_info);
                    // Pre-mark existing accounts as already "linked" so no buttons show
                    const initialStatus = {};
                    res.data.account_info.forEach((a, i) => {
                        if (!a.is_new_account) initialStatus[i] = "linked";
                    });
                    setAccountStatus(initialStatus);
                }
                if (res.data.status === "APPROVE") {
                    setIsApproved(true);
                }
            } catch (err) {
                console.error(err);
                setError("Failed to fetch review data. Ensure the document has been processed.");
            } finally {
                setIsLoading(false);
            }
        };
        fetchReviewData();
    }, [documentId]);

    const handleApprove = async () => {
        setIsApproving(true);
        try {
            await API.post(`/documents/${documentId}/approve`);
            setIsApproved(true);
        } catch (err) {
            console.error(err);
            alert("Approval failed: " + (err.response?.data?.detail || err.message));
        } finally {
            setIsApproving(false);
        }
    };

    // Save a single detected account by its index in accountInfo
    const handleSaveAccount = async (idx) => {
        setAcctStatus(idx, "saving");
        try {
            await API.post(`/documents/${documentId}/confirm-account`, {
                save: true,
                accounts: [accountInfo[idx]],
            });
            setAcctStatus(idx, "saved");
        } catch (err) {
            console.error(err);
            setAcctStatus(idx, null); // reset so user can retry
            alert("Failed to save account: " + (err.response?.data?.detail || err.message));
        }
    };

    // Skip a single detected account — fire-and-forget, non-fatal
    const handleSkipAccount = async (idx) => {
        setAcctStatus(idx, "skipped");
        try {
            await API.post(`/documents/${documentId}/confirm-account`, {
                save: false,
                accounts: [accountInfo[idx]],
            });
        } catch (err) {
            console.error(err); // non-fatal
        }
    };

    const handleDownloadJson = async () => {
        try {
            const res = await API.get(`/documents/${documentId}/download-json`);
            const jsonStr = JSON.stringify(res.data, null, 2);
            const blob = new Blob([jsonStr], { type: "application/json" });
            const url = URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            const safeName = (data?.bank_name || "transactions").replace(/\s+/g, "_");
            a.download = `${safeName}_transactions.json`;
            a.click();
            URL.revokeObjectURL(url);
        } catch (err) {
            console.error(err);
            alert("Download failed: " + (err.response?.data?.detail || err.message));
        }
    };

    if (isLoading) {
        return (
            <AppLayout>
                <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center', height: '60vh' }}>
                    <Loader2 className="spin-icon" size={48} color="#483EA8" />
                </div>
            </AppLayout>
        );
    }

    if (error || !data) {
        return (
            <AppLayout>
                <div style={{ textAlign: 'center', marginTop: '4rem' }}>
                    <h2 style={{ color: '#e74c3c' }}>{error || "Something went wrong"}</h2>
                    <button className="btn-ghost" onClick={() => navigate("/dashboard")} style={{ marginTop: '1rem', color: '#483EA8', borderColor: '#483EA8' }}>
                        Back to Dashboard
                    </button>
                </div>
            </AppLayout>
        );
    }

    /* Transaction table columns — matches DB keys: date, debit, credit, balance, details, confidence */
    const renderTransactionTable = (transactions, title, icon, iconColor) => (
        <div className="review-card" style={{ padding: '1.5rem 0' }}>
            <h3 style={{ fontSize: '0.95rem', marginBottom: '1.25rem', padding: '0 1.5rem', color: '#111827', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
                {icon} {title}
            </h3>
            <div className="table-wrap">
                <table className="review-table premium-table" style={{ width: '100%' }}>
                    <thead>
                        <tr style={{ background: '#f9fafb' }}>
                            <th style={{ paddingLeft: '1.5rem' }}>Date</th>
                            <th>Details</th>
                            <th style={{ textAlign: 'right' }}>Debit</th>
                            <th style={{ textAlign: 'right' }}>Credit</th>
                            <th style={{ textAlign: 'right' }}>Balance</th>
                            <th style={{ textAlign: 'center', paddingRight: '1.5rem' }}>Confidence</th>
                        </tr>
                    </thead>
                    <tbody>
                        {transactions && transactions.length > 0 ? transactions.map((tx, i) => (
                            <tr key={i}>
                                <td style={{ paddingLeft: '1.5rem', whiteSpace: 'nowrap' }}>{tx.date || '-'}</td>
                                <td style={{ maxWidth: '300px', overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{tx.details || '-'}</td>
                                <td style={{ textAlign: 'right', color: tx.debit ? '#e74c3c' : '#d1d5db', fontWeight: tx.debit ? 600 : 400 }}>
                                    {tx.debit ? tx.debit.toLocaleString() : '-'}
                                </td>
                                <td style={{ textAlign: 'right', color: tx.credit ? '#27ae60' : '#d1d5db', fontWeight: tx.credit ? 600 : 400 }}>
                                    {tx.credit ? tx.credit.toLocaleString() : '-'}
                                </td>
                                <td style={{ textAlign: 'right', fontWeight: 600 }}>
                                    {tx.balance != null ? tx.balance.toLocaleString() : '-'}
                                </td>
                                <td style={{ textAlign: 'center', paddingRight: '1.5rem' }}>
                                    <span style={{
                                        background: tx.confidence >= 0.9 ? '#def7ec' : tx.confidence >= 0.7 ? '#fef3c7' : '#fde8e8',
                                        color: tx.confidence >= 0.9 ? '#03543f' : tx.confidence >= 0.7 ? '#92400e' : '#9b1c1c',
                                        padding: '2px 8px',
                                        borderRadius: '50px',
                                        fontSize: '0.7rem',
                                        fontWeight: 700,
                                    }}>
                                        {tx.confidence != null ? (tx.confidence * 100).toFixed(0) + '%' : 'N/A'}
                                    </span>
                                </td>
                            </tr>
                        )) : (
                            <tr><td colSpan="6" style={{ textAlign: 'center', padding: '3rem', color: '#999' }}>No transactions extracted.</td></tr>
                        )}
                    </tbody>
                </table>
            </div>
        </div>
    );

    return (
        <AppLayout>
            <motion.div
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
            >
                <div style={{ marginBottom: '1.5rem' }}>
                    <button
                        onClick={() => navigate(-1)}
                        style={{
                            background: 'none',
                            border: 'none',
                            display: 'flex',
                            alignItems: 'center',
                            gap: '4px',
                            fontSize: '0.875rem',
                            color: '#6b7280',
                            cursor: 'pointer',
                            fontWeight: 600,
                            marginBottom: '0.75rem',
                            padding: 0,
                        }}
                    >
                        <ChevronLeft size={16} /> Back
                    </button>

                    <h2 style={{ fontSize: '1.75rem', fontWeight: 800, color: '#1a1a2e' }}>Review Transactions</h2>
                </div>

                {/* Metadata bar */}
                <div className="review-card" style={{ display: 'flex', alignItems: 'center', gap: '2rem', marginBottom: '1.5rem' }}>
                    <div style={{ display: 'flex', flexDirection: 'column' }}>
                        <label style={{ fontSize: '0.65rem', color: '#999', fontWeight: 600, display: 'flex', alignItems: 'center', gap: '4px' }}>
                            <Building2 size={12} /> Bank Name
                        </label>
                        <span style={{ fontWeight: 700, fontSize: '0.95rem', color: '#111827' }}>{data.bank_name}</span>
                    </div>
                    <div style={{ display: 'flex', flexDirection: 'column' }}>
                        <label style={{ fontSize: '0.65rem', color: '#999', fontWeight: 600 }}>Code Txns</label>
                        <span style={{ fontWeight: 700, fontSize: '0.95rem' }}>{data.code_transactions?.length || 0}</span>
                    </div>
                    <div style={{ display: 'flex', flexDirection: 'column' }}>
                        <label style={{ fontSize: '0.65rem', color: '#999', fontWeight: 600 }}>LLM Txns</label>
                        <span style={{ fontWeight: 700, fontSize: '0.95rem' }}>{data.llm_transactions?.length || 0}</span>
                    </div>
                    <div style={{ marginLeft: 'auto', display: 'flex', alignItems: 'center', gap: '0.75rem' }}>
                        {/* Download JSON button — outlined brand-purple, hover fills solid */}
                        <button
                            onClick={handleDownloadJson}
                            className="download-json-btn"
                            title="Download extracted transactions as JSON"
                        >
                            <Download size={15} /> Download JSON
                        </button>

                        {isApproved ? (
                            <button
                                disabled
                                style={{
                                    padding: '0.5rem 2rem',
                                    background: 'transparent',
                                    color: '#27ae60',
                                    border: '2px solid #27ae60',
                                    borderRadius: '10px',
                                    fontWeight: 700,
                                    fontSize: '0.85rem',
                                    fontFamily: 'inherit',
                                    display: 'inline-flex',
                                    alignItems: 'center',
                                    gap: '8px',
                                    cursor: 'default',
                                    opacity: 0.85,
                                }}
                            >
                                <CheckCircle size={16} /> APPROVED
                            </button>
                        ) : (
                            <button
                                className="download-json-btn"
                                onClick={handleApprove}
                                disabled={isApproving}
                                style={{ padding: '0.5rem 2rem', opacity: isApproving ? 0.65 : 1, cursor: isApproving ? 'not-allowed' : 'pointer' }}
                            >
                                {isApproving ? (
                                    <><Loader2 size={16} className="spin-icon" /> APPROVING...</>
                                ) : (
                                    <><Check size={16} /> APPROVE</>
                                )}
                            </button>
                        )}
                    </div>
                </div>

                {/* ── Account Banner ─────────────────────────────────────────── */}
                {accountInfo.length > 0 && (
                    <div className="review-card" style={{ marginBottom: '1.5rem', padding: '1rem 1.5rem' }}>
                        {accountInfo.map((acct, idx) => {
                            const status = accountStatus[idx]; // null | "saving" | "saved" | "skipped" | "linked"
                            const last4 = acct.account_number_last4 || acct.card_last4;

                            return (
                                <div key={idx} style={{
                                    display: 'flex',
                                    alignItems: 'center',
                                    gap: '1rem',
                                    paddingTop: idx > 0 ? '0.75rem' : 0,
                                    marginTop: idx > 0 ? '0.75rem' : 0,
                                    borderTop: idx > 0 ? '1px solid #f0f0f0' : 'none',
                                }}>
                                    {/* ── Icon ──────────────────────────────── */}
                                    <div style={{
                                        width: 36, height: 36, borderRadius: '50%',
                                        background: acct.is_new_account ? '#ede9ff' : '#e8f5e9',
                                        display: 'flex', alignItems: 'center', justifyContent: 'center',
                                        flexShrink: 0,
                                    }}>
                                        {acct.is_new_account
                                            ? <CreditCard size={16} color="#483EA8" />
                                            : <Link size={16} color="#27ae60" />
                                        }
                                    </div>

                                    {/* ── Account text ──────────────────────── */}
                                    <div style={{ flex: 1, minWidth: 0 }}>
                                        <p style={{
                                            margin: 0,
                                            fontSize: '0.82rem',
                                            fontWeight: 700,
                                            color: acct.is_new_account ? '#111827' : '#27ae60',
                                        }}>
                                            {acct.is_new_account ? 'New account detected' : 'Account linked'}
                                        </p>
                                        <p style={{ margin: 0, fontSize: '0.75rem', color: '#6b7280', marginTop: 2 }}>
                                            {acct.institution_name}
                                            {last4 && (
                                                <span style={{ marginLeft: 6, letterSpacing: '0.08em', fontFamily: 'monospace' }}>
                                                    ••••{last4}
                                                </span>
                                            )}
                                            {acct.suggested_name && acct.is_new_account && (
                                                <span style={{ marginLeft: 8, color: '#9ca3af', fontSize: '0.7rem' }}>
                                                    will be saved as "{acct.suggested_name}"
                                                </span>
                                            )}
                                        </p>
                                    </div>

                                    {/* ── Right side: buttons OR status chip ── */}
                                    {acct.is_new_account ? (
                                        // NEW account — show Save/Skip until actioned
                                        status === "saved" ? (
                                            <span style={{
                                                fontSize: '0.7rem', fontWeight: 700,
                                                color: '#27ae60', background: '#e8f5e9',
                                                padding: '3px 10px', borderRadius: '50px', flexShrink: 0,
                                            }}>
                                                ✓ Saved
                                            </span>
                                        ) : status === "skipped" ? (
                                            <span style={{
                                                fontSize: '0.7rem', fontWeight: 600,
                                                color: '#9ca3af', background: '#f3f4f6',
                                                padding: '3px 10px', borderRadius: '50px', flexShrink: 0,
                                            }}>
                                                Skipped
                                            </span>
                                        ) : (
                                            // Pending — show action buttons
                                            <div style={{ display: 'flex', gap: '0.5rem', flexShrink: 0 }}>
                                                <button
                                                    onClick={() => handleSaveAccount(idx)}
                                                    disabled={status === "saving"}
                                                    style={{
                                                        padding: '0.35rem 1rem',
                                                        background: '#483EA8',
                                                        color: '#fff',
                                                        border: 'none',
                                                        borderRadius: '8px',
                                                        fontWeight: 700,
                                                        fontSize: '0.75rem',
                                                        cursor: status === "saving" ? 'not-allowed' : 'pointer',
                                                        opacity: status === "saving" ? 0.65 : 1,
                                                        fontFamily: 'inherit',
                                                        display: 'inline-flex',
                                                        alignItems: 'center',
                                                        gap: 4,
                                                    }}
                                                >
                                                    {status === "saving"
                                                        ? <><Loader2 size={12} className="spin-icon" /> Saving…</>
                                                        : 'Save Account'
                                                    }
                                                </button>
                                                <button
                                                    onClick={() => handleSkipAccount(idx)}
                                                    disabled={status === "saving"}
                                                    style={{
                                                        padding: '0.35rem 0.85rem',
                                                        background: 'transparent',
                                                        color: '#6b7280',
                                                        border: '1px solid #e5e7eb',
                                                        borderRadius: '8px',
                                                        fontWeight: 600,
                                                        fontSize: '0.75rem',
                                                        cursor: status === "saving" ? 'not-allowed' : 'pointer',
                                                        fontFamily: 'inherit',
                                                    }}
                                                >
                                                    Skip
                                                </button>
                                            </div>
                                        )
                                    ) : (
                                        // EXISTING account — always just a "Linked" chip, no action needed
                                        <span style={{
                                            fontSize: '0.7rem', fontWeight: 700,
                                            color: '#27ae60', background: '#e8f5e9',
                                            padding: '3px 10px', borderRadius: '50px', flexShrink: 0,
                                        }}>
                                            ✓ Linked
                                        </span>
                                    )}
                                </div>
                            );
                        })}
                    </div>
                )}

                {/* Main content: tables on left, JSON on right */}
                <div style={{ display: 'flex', gap: '1.5rem', alignItems: 'flex-start' }}>
                    <div style={{ flex: 2, display: 'flex', flexDirection: 'column', gap: '1.5rem', minWidth: 0 }}>
                        {renderTransactionTable(
                            data.code_transactions,
                            "Extracted by Code",
                            <Code size={18} style={{ color: '#27ae60' }} />,
                            '#27ae60'
                        )}
                        {renderTransactionTable(
                            data.llm_transactions,
                            "Extracted by LLM",
                            <Cpu size={18} style={{ color: '#483EA8' }} />,
                            '#483EA8'
                        )}
                    </div>

                    {/* JSON Column */}
                    <div style={{ flex: 1, position: 'sticky', top: '2rem', minWidth: 0 }}>
                        <div className="review-card">
                            <h3 style={{ fontSize: '0.95rem', marginBottom: '1rem', color: '#111827', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
                                <FileSearch size={18} style={{ color: '#483EA8' }} /> Identifier Config
                            </h3>
                            <p style={{ fontSize: '0.75rem', color: '#6b7280', marginBottom: '1rem' }}>
                                Detected pattern configuration used for this statement format.
                            </p>
                            <pre className="json-view" style={{ minHeight: '400px' }}>
                                {JSON.stringify(data.identifier_json, null, 2)}
                            </pre>
                        </div>
                    </div>
                </div>
            </motion.div>
        </AppLayout>
    );
}