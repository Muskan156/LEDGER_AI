import { useState, useEffect } from "react";
import { motion } from "framer-motion";
import {
    FileText,
    CheckCircle,
    AlertCircle,
    Clock,
    Search,
    ChevronDown,
    Copy,
    Table as TableIcon,
    Loader2 as SpinIcon,
    Trash2
} from "lucide-react";
import AppLayout from "../components/Layout";
import API from "../api/api";
import { useNavigate } from "react-router-dom";

export default function DashboardPage() {
    const [stats, setStats] = useState({ total: 0, parsed: 0, failed: 0, pending_review: 0 });
    const [recentDocs, setRecentDocs] = useState([]);
    const [isLoading, setIsLoading] = useState(true);
    const [sortOption, setSortOption] = useState("Newest first");
    const [isSortOpen, setIsSortOpen] = useState(false);
    const navigate = useNavigate();

    const sortOptions = ["Newest first", "Oldest first", "Last activity", "Alphabetically"];

    useEffect(() => {
        fetchData();
    }, []);

    const fetchData = async () => {
        setIsLoading(true);
        try {
            const [statsRes, recentRes] = await Promise.all([
                API.get("/documents/stats"),
                API.get("/documents/recent")
            ]);
            setStats(statsRes.data);
            setRecentDocs(recentRes.data);
        } catch (err) {
            console.error("Failed to fetch dashboard data", err);
        } finally {
            setIsLoading(false);
        }
    };

    const sortedDocs = [...recentDocs].sort((a, b) => {
        if (sortOption === "Newest first") return new Date(b.created_at) - new Date(a.created_at);
        if (sortOption === "Oldest first") return new Date(a.created_at) - new Date(b.created_at);
        if (sortOption === "Alphabetically") return a.file_name.localeCompare(b.file_name);
        return 0; // default
    });

    const handleDeleteDocument = async (docId, fileName) => {
        const confirmed = window.confirm(`Are you sure you want to delete "${fileName}"? This action cannot be undone.`);
        if (!confirmed) return;
        try {
            await API.delete(`/documents/${docId}`);
            setRecentDocs(prev => prev.filter(d => d.document_id !== docId));
            // Refresh stats as well
            const statsRes = await API.get("/documents/stats");
            setStats(statsRes.data);
        } catch (err) {
            console.error("Delete failed", err);
            alert("Failed to delete document: " + (err.response?.data?.detail || err.message));
        }
    };

    const statCards = [
        { label: "Total Uploads", value: stats.total, icon: FileText, color: "#483EA8" },
        { label: "Successfully Parsed", value: stats.parsed, icon: CheckCircle, color: "#27ae60" },
        { label: "Failed/Corrupted", value: stats.failed, icon: AlertCircle, color: "#e74c3c" },
        { label: "Pending Review", value: stats.pending_review, icon: Clock, color: "#f39c12" },
    ];

    const formatTime = (dateStr) => {
        const date = new Date(dateStr);
        const now = new Date();
        const diff = Math.floor((now - date) / 1000);
        if (diff < 60) return "Just now";
        if (diff < 3600) return `${Math.floor(diff / 60)}m ago`;
        if (diff < 86400) return `${Math.floor(diff / 3600)}h ago`;
        return date.toLocaleDateString();
    };

    return (
        <AppLayout>
            <motion.div
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
            >
                <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '2.5rem' }}>
                    <h2 style={{ fontSize: '1.75rem', fontWeight: 800, color: '#1a1a2e' }}>Dashboard</h2>
                </div>

                <div className="review-grid" style={{ gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))', gap: '1.5rem', marginBottom: '3.5rem' }}>
                    {statCards.map((stat, i) => (
                        <div key={i} className="review-card" style={{ display: 'flex', alignItems: 'center', gap: '1.25rem', padding: '1.5rem' }}>
                            <div style={{ padding: '0.85rem', borderRadius: '14px', background: `${stat.color}12`, color: stat.color }}>
                                <stat.icon size={26} />
                            </div>
                            <div className="meta-item">
                                <label style={{ fontSize: '0.75rem', color: '#666', fontWeight: 600 }}>{stat.label}</label>
                                <span style={{ fontSize: '1.5rem', fontWeight: 800 }}>{stat.value}</span>
                            </div>
                        </div>
                    ))}
                </div>

                <div className="recent-activities-header" style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '1.5rem' }}>
                    <div style={{ display: 'flex', alignItems: 'center', gap: '1rem', position: 'relative' }}>
                        <span style={{ fontSize: '0.875rem', fontWeight: 600, color: '#4b5563' }}>Sort:</span>
                        <div
                            style={{ display: 'flex', alignItems: 'center', gap: '0.4rem', fontSize: '0.875rem', cursor: 'pointer', fontWeight: 700, color: '#483EA8' }}
                            onClick={() => setIsSortOpen(!isSortOpen)}
                        >
                            {sortOption} <ChevronDown size={14} />
                        </div>
                        {isSortOpen && (
                            <div style={{
                                position: 'absolute',
                                top: '100%',
                                left: '40px',
                                background: 'white',
                                border: '1px solid #eee',
                                borderRadius: '8px',
                                boxShadow: '0 10px 25px rgba(0,0,0,0.1)',
                                zIndex: 10,
                                width: '180px',
                                padding: '0.5rem 0'
                            }}>
                                {sortOptions.map(opt => (
                                    <div
                                        key={opt}
                                        onClick={() => { setSortOption(opt); setIsSortOpen(false); }}
                                        style={{
                                            padding: '0.6rem 1rem',
                                            fontSize: '0.8rem',
                                            cursor: 'pointer',
                                            background: sortOption === opt ? '#f0eeff' : 'transparent',
                                            color: sortOption === opt ? '#483EA8' : '#4b5563',
                                            fontWeight: sortOption === opt ? 700 : 500
                                        }}
                                    >
                                        {opt}
                                    </div>
                                ))}
                            </div>
                        )}
                    </div>
                </div>

                <div className="review-card" style={{ padding: '0' }}>
                    <div className="table-wrap">
                        <table className="review-table premium-table">
                            <thead>
                                <tr style={{ background: 'transparent' }}>
                                    <th style={{ paddingLeft: '2rem' }}>Name</th>
                                    <th style={{ textAlign: 'center' }}>Documents</th>
                                    <th style={{ textAlign: 'center' }}>Parsed</th>
                                    <th style={{ textAlign: 'center' }}>Failed</th>
                                    <th style={{ textAlign: 'center' }}>Type</th>
                                    <th style={{ textAlign: 'center' }}>Last activity</th>
                                    <th style={{ textAlign: 'center', paddingRight: '2rem' }}>Actions</th>
                                </tr>
                            </thead>
                            <tbody>
                                {isLoading ? (
                                    <tr>
                                        <td colSpan="7" style={{ textAlign: 'center', padding: '3rem' }}>
                                            <div className="spin-icon" style={{ display: 'inline-block' }}><SpinIcon size={24} color="#483EA8" /></div>
                                        </td>
                                    </tr>
                                ) : sortedDocs.length === 0 ? (
                                    <tr>
                                        <td colSpan="7" style={{ textAlign: 'center', padding: '3rem', color: '#999' }}>
                                            No documents found.
                                        </td>
                                    </tr>
                                ) : (
                                    sortedDocs.map((doc) => (
                                        <tr key={doc.document_id}>
                                            <td style={{ paddingLeft: '2rem', padding: '1.25rem 2rem' }}>
                                                <div style={{ fontWeight: 700, color: '#111827', fontSize: '0.9rem' }}>{doc.file_name}</div>
                                                <div style={{ fontSize: '0.75rem', color: '#6b7280', marginTop: '4px' }}>
                                                    {doc.institution_name || 'Bank Statement'}
                                                </div>
                                            </td>
                                            <td style={{ textAlign: 'center', fontWeight: 600 }}>1</td>
                                            <td style={{ textAlign: 'center' }}>
                                                <span style={{
                                                    display: 'inline-flex',
                                                    alignItems: 'center',
                                                    justifyContent: 'center',
                                                    width: '24px',
                                                    height: '24px',
                                                    borderRadius: '50%',
                                                    background: doc.status === 'APPROVE' ? '#def7ec' : '#f3f4f6',
                                                    color: doc.status === 'APPROVE' ? '#03543f' : '#9ca3af',
                                                    fontSize: '0.75rem',
                                                    fontWeight: 700
                                                }}>
                                                    {doc.status === 'APPROVE' ? '1' : '0'}
                                                </span>
                                            </td>
                                            <td style={{ textAlign: 'center', color: '#9ca3af' }}>
                                                {doc.status === 'FAILED' ? '1' : '-'}
                                            </td>
                                            <td style={{ textAlign: 'center' }}>
                                                <span style={{
                                                    background: '#fef3c7',
                                                    color: '#92400e',
                                                    padding: '2px 10px',
                                                    borderRadius: '50px',
                                                    fontSize: '0.65rem',
                                                    fontWeight: 800,
                                                    textTransform: 'uppercase'
                                                }}>
                                                    {doc.transaction_parsed_type === 'LLM' ? 'AI' : doc.transaction_parsed_type || 'N/A'}
                                                </span>
                                            </td>
                                            <td style={{ textAlign: 'center', color: '#4b5563', fontSize: '0.8rem' }}>
                                                {formatTime(doc.created_at)}
                                            </td>
                                            <td style={{ textAlign: 'center', paddingRight: '2rem' }}>
                                                <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', gap: '0.5rem' }}>
                                                    <button
                                                        onClick={() => navigate(`/review?id=${doc.document_id}`)}
                                                        style={{
                                                            background: 'none',
                                                            border: '1px solid #e5e7eb',
                                                            padding: '6px 12px',
                                                            borderRadius: '6px',
                                                            fontSize: '0.75rem',
                                                            fontWeight: 600,
                                                            color: '#483EA8',
                                                            display: 'inline-flex',
                                                            alignItems: 'center',
                                                            gap: '4px',
                                                            cursor: 'pointer',
                                                            transition: 'all 0.2s'
                                                        }}
                                                    >
                                                        <TableIcon size={14} /> Transactions
                                                    </button>
                                                    <button
                                                        onClick={() => handleDeleteDocument(doc.document_id, doc.file_name)}
                                                        title="Delete document"
                                                        style={{
                                                            background: 'none',
                                                            border: '1px solid #fecaca',
                                                            padding: '6px 8px',
                                                            borderRadius: '6px',
                                                            fontSize: '0.75rem',
                                                            fontWeight: 600,
                                                            color: '#e74c3c',
                                                            display: 'inline-flex',
                                                            alignItems: 'center',
                                                            cursor: 'pointer',
                                                            transition: 'all 0.2s'
                                                        }}
                                                        onMouseEnter={e => { e.currentTarget.style.background = '#fef2f2'; }}
                                                        onMouseLeave={e => { e.currentTarget.style.background = 'none'; }}
                                                    >
                                                        <Trash2 size={14} />
                                                    </button>
                                                </div>
                                            </td>
                                        </tr>
                                    ))
                                )}
                            </tbody>
                        </table>
                    </div>
                </div>
            </motion.div>
        </AppLayout>
    );
}
