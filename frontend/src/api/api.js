import axios from "axios";

const API = axios.create({
    baseURL: "http://localhost:8000", // FastAPI backend
});

// Attach JWT token automatically
API.interceptors.request.use((config) => {
    const token = localStorage.getItem("token");
    if (token) {
        config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
});

// Handle 401 errors (token expired or invalid)
API.interceptors.response.use(
    (response) => response,
    (error) => {
        if (error.response && error.response.status === 401) {
            console.error("401 Unauthorized - redirecting to login");
            localStorage.removeItem("token");
            window.location.href = "/";
        }
        return Promise.reject(error);
    }
);

export default API;
