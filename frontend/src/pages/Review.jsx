import { useState, useEffect } from "react";
import { useSearchParams, useNavigate } from "react-router-dom";
import { motion } from "framer-motion";
import { Check, Code, FileSearch, Building2, Cpu, Loader2, ChevronLeft, CheckCircle } from "lucide-react";
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
                // If the document was already approved, reflect that in UI
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
                <div style={{ marginBottom: '1.5rem', display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
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
                            fontWeight: 600
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
                    <div style={{ marginLeft: 'auto' }}>
                        {isApproved ? (
                            <button
                                disabled
                                style={{
                                    padding: '0.5rem 2rem',
                                    marginTop: 0,
                                    background: 'linear-gradient(135deg, #27ae60, #2ecc71)',
                                    color: 'white',
                                    border: 'none',
                                    borderRadius: '10px',
                                    fontWeight: 700,
                                    fontSize: '0.85rem',
                                    display: 'inline-flex',
                                    alignItems: 'center',
                                    gap: '8px',
                                    cursor: 'default',
                                    opacity: 0.95,
                                    boxShadow: '0 2px 8px rgba(39,174,96,0.3)',
                                }}
                            >
                                <CheckCircle size={16} /> APPROVED
                            </button>
                        ) : (
                            <button
                                className="btn-submit"
                                onClick={handleApprove}
                                disabled={isApproving}
                                style={{ padding: '0.5rem 2rem', marginTop: 0, opacity: isApproving ? 0.7 : 1 }}
                            >
                                {isApproving ? (
                                    <><Loader2 size={16} className="spin-icon" style={{ marginRight: '8px' }} /> APPROVING...</>
                                ) : (
                                    <><Check size={16} style={{ marginRight: '8px' }} /> APPROVE</>
                                )}
                            </button>
                        )}
                    </div>
                </div>

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
