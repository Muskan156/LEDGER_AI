import { NavLink, useNavigate } from "react-router-dom";
import { LayoutDashboard, FileUp, LogOut } from "lucide-react";

export default function AppLayout({ children }) {
    const navigate = useNavigate();
    const userEmail = "user@gmail.com"; // Mock user for now

    const handleLogout = () => {
        localStorage.removeItem("token");
        navigate("/");
    };

    return (
        <div className="app-container">
            <aside className="sidebar">
                <div className="sidebar-logo">
                    <h1>LEDGER AI</h1>
                    <p>{userEmail}</p>
                </div>

                <nav className="nav-links">
                    <NavLink
                        to="/dashboard"
                        className={({ isActive }) => isActive ? "nav-item nav-item--active" : "nav-item"}
                    >
                        <LayoutDashboard size={20} />
                        Dashboard
                    </NavLink>
                    <NavLink
                        to="/upload"
                        className={({ isActive }) => isActive ? "nav-item nav-item--active" : "nav-item"}
                    >
                        <FileUp size={20} />
                        Extract PDF
                    </NavLink>
                </nav>

                <div className="logout-btn">
                    <button onClick={handleLogout} className="nav-item" style={{ width: '100%', background: 'none', border: 'none', cursor: 'pointer' }}>
                        <LogOut size={20} />
                        Logout
                    </button>
                </div>
            </aside>

            <main className="main-content">
                {children}
            </main>
        </div>
    );
}
