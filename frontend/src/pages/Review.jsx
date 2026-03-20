import { useState, useEffect } from "react";
import { useSearchParams, useNavigate } from "react-router-dom";
import { motion } from "framer-motion";
import { Check, Code, FileSearch, Building2, Cpu, Loader2, ChevronLeft, CheckCircle, Download, Link } from "lucide-react";
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
    const [userAccounts, setUserAccounts] = useState([]);     // all user's accounts for dropdown
    const [selectedAccountId, setSelectedAccountId] = useState(null); // currently chosen
    const [isLinkingAccount, setIsLinkingAccount] = useState(false);
    const [accountLinked, setAccountLinked] = useState(false);

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
                // Populate account dropdown
                if (res.data.user_accounts) {
                    setUserAccounts(res.data.user_accounts);
                }
                // If document already has a linked account, pre-select it
                if (res.data.selected_account_id) {
                    setSelectedAccountId(res.data.selected_account_id);
                    setAccountLinked(true);
                }
                if (res.data.status === "APPROVE") {
                    setIsApproved(true);
                    setAccountLinked(true);
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

    const handleLinkAccount = async () => {
        if (!selectedAccountId) return;
        setIsLinkingAccount(true);
        try {
            await API.post(`/documents/${documentId}/select-account`, {
                account_id: selectedAccountId,
            });
            setAccountLinked(true);
        } catch (err) {
            console.error(err);
            alert("Failed to link account: " + (err.response?.data?.detail || err.message));
        } finally {
            setIsLinkingAccount(false);
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

                {/* ── Account Selector ────────────────────────────────────────── */}
                <div className="review-card" style={{ marginBottom: '1.5rem', padding: '1rem 1.5rem' }}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: '1rem' }}>
                        {/* Icon */}
                        <div style={{
                            width: 36, height: 36, borderRadius: '50%',
                            background: accountLinked ? '#e8f5e9' : '#ede9ff',
                            display: 'flex', alignItems: 'center', justifyContent: 'center',
                            flexShrink: 0,
                        }}>
                            <Link size={16} color={accountLinked ? '#27ae60' : '#483EA8'} />
                        </div>

                        {/* Label + dropdown */}
                        <div style={{ flex: 1, minWidth: 0 }}>
                            <p style={{ margin: 0, fontSize: '0.75rem', fontWeight: 600, color: '#6b7280', marginBottom: 4 }}>
                                Link this document to an account
                            </p>
                            {userAccounts.length > 0 ? (
                                <select
                                    value={selectedAccountId || ""}
                                    onChange={e => {
                                        setSelectedAccountId(Number(e.target.value) || null);
                                        setAccountLinked(false);
                                    }}
                                    disabled={isApproved}
                                    style={{
                                        width: '100%',
                                        maxWidth: 360,
                                        padding: '0.4rem 0.6rem',
                                        fontSize: '0.82rem',
                                        fontWeight: 600,
                                        color: '#111827',
                                        border: '1.5px solid #e5e7eb',
                                        borderRadius: '8px',
                                        background: '#fff',
                                        fontFamily: 'inherit',
                                        cursor: isApproved ? 'not-allowed' : 'pointer',
                                        outline: 'none',
                                    }}
                                >
                                    <option value="">— Select account —</option>
                                    {userAccounts.map(acct => {
                                        const last4 = acct.account_number_last4 || acct.card_last4;
                                        const label = last4
                                            ? `${acct.account_name}  ••••${last4}`
                                            : acct.account_name;
                                        return (
                                            <option key={acct.account_id} value={acct.account_id}>
                                                {label}
                                            </option>
                                        );
                                    })}
                                </select>
                            ) : (
                                <p style={{ margin: 0, fontSize: '0.78rem', color: '#9ca3af' }}>
                                    No accounts added yet. Add accounts from the Dashboard.
                                </p>
                            )}
                        </div>

                        {/* Link button or Linked chip */}
                        {accountLinked ? (
                            <span style={{
                                fontSize: '0.7rem', fontWeight: 700,
                                color: '#27ae60', background: '#e8f5e9',
                                padding: '3px 10px', borderRadius: '50px', flexShrink: 0,
                            }}>
                                ✓ Linked
                            </span>
                        ) : (
                            <button
                                onClick={handleLinkAccount}
                                disabled={!selectedAccountId || isLinkingAccount || isApproved}
                                style={{
                                    padding: '0.4rem 1.1rem',
                                    background: selectedAccountId ? '#483EA8' : '#e5e7eb',
                                    color: selectedAccountId ? '#fff' : '#9ca3af',
                                    border: 'none',
                                    borderRadius: '8px',
                                    fontWeight: 700,
                                    fontSize: '0.78rem',
                                    fontFamily: 'inherit',
                                    cursor: selectedAccountId && !isApproved ? 'pointer' : 'not-allowed',
                                    display: 'inline-flex',
                                    alignItems: 'center',
                                    gap: 4,
                                    flexShrink: 0,
                                    transition: 'background 0.15s',
                                }}
                            >
                                {isLinkingAccount
                                    ? <><Loader2 size={12} className="spin-icon" /> Linking…</>
                                    : <><Link size={12} /> Link</>
                                }
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