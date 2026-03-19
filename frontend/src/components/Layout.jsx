import { NavLink, useNavigate } from "react-router-dom";
import { LayoutDashboard, FileUp, LogOut } from "lucide-react";

export default function AppLayout({ children }) {
    const navigate = useNavigate();
    const userEmail = localStorage.getItem("userEmail") || "user@gmail.com";

    const handleLogout = () => {
        localStorage.removeItem("token");
        localStorage.removeItem("userEmail");
        navigate("/");
    };

    return (
        <div className="app-container">
            {/* ── Top Header ── */}
            <header className="top-header">
                <div className="top-header-inner">
                    {/* Brand */}
                    <div className="header-brand">
                        <span className="header-brand-dot" />
                        <span className="header-brand-name">LEDGER AI</span>
                    </div>

                    {/* Nav links */}
                    <nav className="header-nav">
                        <NavLink
                            to="/dashboard"
                            className={({ isActive }) =>
                                isActive ? "header-nav-item header-nav-item--active" : "header-nav-item"
                            }
                        >
                            <LayoutDashboard size={16} />
                            Dashboard
                        </NavLink>
                        <NavLink
                            to="/upload"
                            className={({ isActive }) =>
                                isActive ? "header-nav-item header-nav-item--active" : "header-nav-item"
                            }
                        >
                            <FileUp size={16} />
                            Extract PDF
                        </NavLink>
                    </nav>

                    {/* User + Logout */}
                    <div className="header-user">
                        <span className="header-email">{userEmail}</span>
                        <button className="header-logout-btn" onClick={handleLogout}>
                            <LogOut size={15} />
                            Logout
                        </button>
                    </div>
                </div>
            </header>

            {/* ── Page content ── */}
            <main className="main-content">
                <div className="page-inner">
                    {children}
                </div>
            </main>
        </div>
    );
}
