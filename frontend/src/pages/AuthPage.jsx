import { useState } from "react";
import { useNavigate } from "react-router-dom";
import { motion, AnimatePresence } from "framer-motion";
import { Loader2, AlertCircle, CheckCircle2 } from "lucide-react";
import API from "../api/api";

/* ─────────────────────────────────────────────────────────
   AuthPage — Split-panel card that toggles between
   Sign In (left) / Sign Up (right) just like the template.
───────────────────────────────────────────────────────── */
export default function AuthPage() {
    const [mode, setMode] = useState("login"); // "login" | "register"

    // Login state
    const [loginEmail, setLoginEmail] = useState("");
    const [loginPassword, setLoginPassword] = useState("");
    const [loginLoading, setLoginLoading] = useState(false);
    const [loginError, setLoginError] = useState("");

    // Register state
    const [regEmail, setRegEmail] = useState("");
    const [regPassword, setRegPassword] = useState("");
    const [regLoading, setRegLoading] = useState(false);
    const [regError, setRegError] = useState("");
    const [regSuccess, setRegSuccess] = useState(false);

    const navigate = useNavigate();

    /* ── Login ── */
    const handleLogin = async (e) => {
        e.preventDefault();
        setLoginError("");
        setLoginLoading(true);
        try {
            const body = new URLSearchParams();
            body.append("username", loginEmail);
            body.append("password", loginPassword);

            const res = await API.post("/auth/login", body, {
                headers: { "Content-Type": "application/x-www-form-urlencoded" },
            });

            localStorage.setItem("token", res.data.access_token);
            navigate("/upload");
        } catch (err) {
            setLoginError(
                err.response?.data?.detail || "Invalid credentials. Please try again."
            );
        } finally {
            setLoginLoading(false);
        }
    };

    /* ── Register ── */
    const handleRegister = async (e) => {
        e.preventDefault();
        setRegError("");
        if (regPassword.length < 8) {
            setRegError("Password must be at least 8 characters.");
            return;
        }
        setRegLoading(true);
        try {
            await API.post("/auth/register", { email: regEmail, password: regPassword });
            setRegSuccess(true);
            setTimeout(() => {
                setRegSuccess(false);
                setRegEmail("");
                setRegPassword("");
                setMode("login");
            }, 2000);
        } catch (err) {
            setRegError(
                err.response?.data?.detail || "Registration failed. Please try again."
            );
        } finally {
            setRegLoading(false);
        }
    };

    const toRegister = () => { setLoginError(""); setMode("register"); };
    const toLogin = () => { setRegError(""); setRegSuccess(false); setMode("login"); };

    return (
        <div className="auth-wrapper">
            <div className="auth-card">
                <AnimatePresence mode="wait" initial={false}>
                    {mode === "login" ? (
                        /* ===== LOGIN VIEW ===== */
                        <motion.div
                            key="login-card"
                            initial={{ opacity: 0 }}
                            animate={{ opacity: 1 }}
                            exit={{ opacity: 0 }}
                            transition={{ duration: 0.3 }}
                            style={{ display: "contents" }}
                        >
                            {/* LEFT — White form panel */}
                            <div className="auth-panel auth-panel--form">
                                <h2 className="form-title">Sign In</h2>

                                <form className="auth-form" onSubmit={handleLogin}>
                                    <AnimatePresence mode="wait">
                                        {loginError && (
                                            <motion.div
                                                key="err"
                                                className="auth-alert auth-alert--error"
                                                initial={{ opacity: 0, y: -4 }}
                                                animate={{ opacity: 1, y: 0 }}
                                                exit={{ opacity: 0, y: -4 }}
                                            >
                                                <AlertCircle size={14} />
                                                <span>{loginError}</span>
                                            </motion.div>
                                        )}
                                    </AnimatePresence>

                                    <input
                                        type="email"
                                        className="auth-input"
                                        placeholder="Email"
                                        value={loginEmail}
                                        onChange={(e) => setLoginEmail(e.target.value)}
                                        required
                                    />
                                    <input
                                        type="password"
                                        className="auth-input"
                                        placeholder="Password"
                                        value={loginPassword}
                                        onChange={(e) => setLoginPassword(e.target.value)}
                                        required
                                    />

                                    <button type="submit" className="btn-submit" disabled={loginLoading}>
                                        {loginLoading ? (
                                            <Loader2 className="spin-icon" size={18} />
                                        ) : (
                                            "Sign In"
                                        )}
                                    </button>
                                </form>
                            </div>

                            {/* RIGHT — Brand panel */}
                            <div className="auth-panel auth-panel--side">
                                <h2 className="side-title">Hello, User!</h2>
                                <p className="side-subtitle">
                                    Enter your personal details and start your journey with us
                                </p>
                                <button className="btn-ghost" onClick={toRegister}>
                                    Sign Up
                                </button>
                            </div>
                        </motion.div>
                    ) : (
                        /* ===== REGISTER VIEW ===== */
                        <motion.div
                            key="register-card"
                            initial={{ opacity: 0 }}
                            animate={{ opacity: 1 }}
                            exit={{ opacity: 0 }}
                            transition={{ duration: 0.3 }}
                            style={{ display: "contents" }}
                        >
                            {/* LEFT — Brand panel */}
                            <div className="auth-panel auth-panel--side">
                                <h2 className="side-title">Welcome Back!</h2>
                                <p className="side-subtitle">
                                    To keep connected with us please login with your personal info
                                </p>
                                <button className="btn-ghost" onClick={toLogin}>
                                    Sign In
                                </button>
                            </div>

                            {/* RIGHT — White form panel */}
                            <div className="auth-panel auth-panel--form">
                                <h2 className="form-title">Create Account</h2>

                                <form className="auth-form" onSubmit={handleRegister}>
                                    <AnimatePresence mode="wait">
                                        {regError && (
                                            <motion.div
                                                key="reg-err"
                                                className="auth-alert auth-alert--error"
                                                initial={{ opacity: 0, y: -4 }}
                                                animate={{ opacity: 1, y: 0 }}
                                                exit={{ opacity: 0, y: -4 }}
                                            >
                                                <AlertCircle size={14} />
                                                <span>{regError}</span>
                                            </motion.div>
                                        )}
                                        {regSuccess && (
                                            <motion.div
                                                key="reg-ok"
                                                className="auth-alert auth-alert--success"
                                                initial={{ opacity: 0, y: -4 }}
                                                animate={{ opacity: 1, y: 0 }}
                                            >
                                                <CheckCircle2 size={14} />
                                                <span>Account created! Redirecting…</span>
                                            </motion.div>
                                        )}
                                    </AnimatePresence>

                                    <input
                                        type="email"
                                        className="auth-input"
                                        placeholder="Email"
                                        value={regEmail}
                                        onChange={(e) => setRegEmail(e.target.value)}
                                        required
                                        disabled={regSuccess}
                                    />
                                    <input
                                        type="password"
                                        className="auth-input"
                                        placeholder="Password"
                                        value={regPassword}
                                        onChange={(e) => setRegPassword(e.target.value)}
                                        required
                                        disabled={regSuccess}
                                        minLength={8}
                                    />

                                    <button
                                        type="submit"
                                        className="btn-submit"
                                        disabled={regLoading || regSuccess}
                                    >
                                        {regLoading ? (
                                            <Loader2 className="spin-icon" size={18} />
                                        ) : regSuccess ? (
                                            <><CheckCircle2 size={16} /> Done!</>
                                        ) : (
                                            "Sign Up"
                                        )}
                                    </button>
                                </form>
                            </div>
                        </motion.div>
                    )}
                </AnimatePresence>
            </div>
        </div>
    );
}
