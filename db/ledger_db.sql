#=====================================DATABASE=================================================
CREATE DATABASE ledgerAI_db;
USE ledgerAI_db;

#=====================================MASTER TABLES=============================================
#======================================1. USER TABLE============================================
/* Purpose: Registered system users */
CREATE TABLE users(
    user_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    email VARCHAR(150) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL,
    status ENUM('ACTIVE','INACTIVE','SUSPENDED') NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP NULL
);

#======================================2. USER SESSION TABLE============================================
/* Purpose: Active login sessions */
CREATE TABLE user_sessions(
    session_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    token VARCHAR(255) NOT NULL UNIQUE,
    expires_at TIMESTAMP NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

#======================================3. USER ACCOUNT GROUP TABLE============================================
/* Purpose: store accounts group */
CREATE TABLE account_groups(
    account_group_id BIGINT PRIMARY KEY AUTO_INCREMENT,
    group_name VARCHAR(100) NOT NULL,
    parent_group_id BIGINT NULL,
    balance_nature ENUM('DR', 'CR') NOT NULL,
    is_profit_loss BOOLEAN NOT NULL DEFAULT FALSE,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created_by BIGINT NOT NULL,
    updated_at TIMESTAMP NULL DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,
    updated_by BIGINT NULL,
    FOREIGN KEY (parent_group_id) REFERENCES account_groups(account_group_id) -- self referencing
);

#======================================4. USER ACCOUNT TABLE============================================
/* Purpose: Personal financial accounts */
CREATE TABLE accounts(
    account_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    account_group_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    account_number VARCHAR(30) UNIQUE,
	account_name VARCHAR(150) NOT NULL,
    opening_balance DECIMAL(18,2) NOT NULL DEFAULT 0.00,
    opening_balance_date DATE NOT NULL,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by BIGINT NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    updated_by BIGINT,
    FOREIGN KEY (account_group_id) REFERENCES account_groups(account_group_id),
    FOREIGN KEY (user_id) REFERENCES users(user_id)
);

#======================================4. MERCHANTS TABLE============================================
/* Purpose: Normalized merchant master */
CREATE TABLE merchants(
    merchant_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    merchant_name VARCHAR(150) NOT NULL UNIQUE,
    normalization_pattern VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

#======================================5. CATEGORIES TABLE============================================
/* Purpose: Income & expense categories */
CREATE TABLE categories(
    category_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    category_name VARCHAR(100) NOT NULL UNIQUE,
    category_type ENUM('INCOME','EXPENSE') NOT NULL, 
    is_active BOOLEAN NOT NULL DEFAULT TRUE
);

#=====================================DOCUMENT TABLES=============================================
#=====================================1. DOCUMENT TYPE=============================================
/* Purpose: Uploaded documents type */
CREATE TABLE document_types(
    document_type_id INT AUTO_INCREMENT PRIMARY KEY,
    type_code VARCHAR(50) NOT NULL UNIQUE
);
INSERT INTO document_types (type_code)
VALUES ('BANK_STATEMENT');

#=====================================2. DOCUMENT TABLE=============================================
/* Purpose: Uploaded PDF documents */
CREATE TABLE documents(
    document_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    document_type_id INT NOT NULL,
    file_name VARCHAR(255) NOT NULL,
    file_path VARCHAR(500) NOT NULL,
    is_password_protected BOOLEAN NOT NULL DEFAULT FALSE,
    status ENUM('UPLOADED','PROCESSING','FAILED','COMPLETED') NOT NULL DEFAULT 'UPLOADED',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
    FOREIGN KEY (document_type_id) REFERENCES document_types(document_type_id)
);

#=====================================3. BANK STATEMENT DETAILS TABLE=============================================
/* Purpose: Bank-specific metadata */
#=====================================STATEMENT UPLOAD FORMAT TABLE=============================================
CREATE TABLE statement_categories (
    statement_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    statement_type ENUM('BANK_STATEMENT','CREDIT_CARD','OTHER') NOT NULL,
    format_name VARCHAR(150) NOT NULL,
    institution_name VARCHAR(100) NOT NULL,   
    ifsc_code VARCHAR(20) NULL,               
    statement_identifier JSON NOT NULL,       
    extraction_logic LONGTEXT NOT NULL,       
    match_threshold DECIMAL(5,2) DEFAULT 65.00,
    logic_version INT DEFAULT 1,
    status ENUM('ACTIVE','UNDER_REVIEW','DISABLED') DEFAULT 'UNDER_REVIEW',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP 
        ON UPDATE CURRENT_TIMESTAMP,
    CONSTRAINT unique_format UNIQUE(statement_type, ifsc_code)
);

CREATE TABLE bank_statement_details(
    statement_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL UNIQUE,
    bank_name VARCHAR(100) NOT NULL,
    statement_start_date DATE NOT NULL,
    statement_end_date DATE NOT NULL,
    CHECK (statement_end_date >= statement_start_date),
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

#=====================================4. ENCRYPTED DOC PASSWORD TABLE=============================================
/* Purpose: Store encrypted doc password */
CREATE TABLE document_password(
	document_id BIGINT PRIMARY KEY,
    encrypted_password VARCHAR(255),
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

#=====================================5. DOCUMENT UPLOAD AUDIT TABLE=============================================
/* Purpose: Store Upload Lifecycle entry*/
CREATE TABLE document_upload_audit(
    audit_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    status ENUM('UPLOADED','PROCESSING','FAILED','COMPLETED') NOT NULL,
    error_message VARCHAR(500),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

#=====================================6. DOCUMENT TEXT EXTRACTION TABLE=============================================
/* Purpose: Store Raw extracted text*/
CREATE TABLE document_text_extractions(
    text_extraction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    extraction_method ENUM('PDF_TEXT','OCR','HYBRID') NOT NULL DEFAULT 'PDF_TEXT',
    extracted_text LONGTEXT NOT NULL,
    extraction_status ENUM('SUCCESS','FAILED') NOT NULL DEFAULT 'SUCCESS',
    error_message VARCHAR(500),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

#=====================================AI PIPELINE & REVIEW TABLES=============================================
#=====================================1. AI EXTRACTION TABLE=============================================
/* Purpose: AI execution tracking*/
CREATE TABLE ai_extraction_jobs(
    ai_job_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    text_extraction_id BIGINT NOT NULL,
    model_version_id INT NOT NULL,
    user_id BIGINT NOT NULL,
    job_status ENUM('PENDING','RUNNING','FAILED','COMPLETED') NOT NULL DEFAULT 'PENDING',
    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP,
    error_message VARCHAR(500),
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(user_id),
    FOREIGN KEY (text_extraction_id) REFERENCES document_text_extractions(text_extraction_id) ON DELETE CASCADE
);

#=====================================2. AI TRANSCATION STAGING TABLE=============================================
/* Purpose: Immutable AI-extracted transactions*/
CREATE TABLE ai_transactions_staging(
    staging_transaction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ai_job_id BIGINT NOT NULL,
    document_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    transaction_json JSON NOT NULL,
    overall_confidence DECIMAL(5,2) NOT NULL,
    review_status ENUM('PENDING','APPROVED','REJECTED') NOT NULL DEFAULT 'PENDING',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id),
    FOREIGN KEY (ai_job_id) REFERENCES ai_extraction_jobs(ai_job_id) ON DELETE CASCADE,
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

#=====================================3. AI TRANSCATION REVIEWS TABLE=============================================
/* Purpose: Human review decisions*/
CREATE TABLE transaction_reviews(
   review_id BIGINT AUTO_INCREMENT PRIMARY KEY,
   staging_transaction_id BIGINT NOT NULL,
   reviewer_user_id	BIGINT NOT NULL,
   review_status ENUM('APPROVED','REJECTED','NEEDS_CLARIFICATION') NOT NULL,
   review_notes	VARCHAR(500),
   reviewed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
   FOREIGN KEY (staging_transaction_id) REFERENCES ai_transactions_staging(staging_transaction_id) ON DELETE CASCADE,
   FOREIGN KEY (reviewer_user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

#=====================================4. AI TRANSCATION REVIEWS OVERRIDE TABLE=============================================
/* Purpose: AI vs Human review changes*/
CREATE TABLE transaction_overrides(
   override_id BIGINT AUTO_INCREMENT PRIMARY KEY,
   staging_transaction_id BIGINT NOT NULL,
   field_name VARCHAR(50) NOT NULL,
   ai_value	VARCHAR(255),
   user_value VARCHAR(255) NOT NULL,
   overridden_by BIGINT NOT NULL,
   overridden_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
   FOREIGN KEY (staging_transaction_id) REFERENCES ai_transactions_staging(staging_transaction_id) ON DELETE CASCADE,
   FOREIGN KEY (overridden_by) REFERENCES users(user_id) ON DELETE CASCADE
);

#=====================================5. TRANSCATION TABLE=============================================
/* Purpose: Final accounting ledger*/
CREATE TABLE transactions(
    transaction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    account_id BIGINT NOT NULL,
    transaction_date DATE NOT NULL,
    amount DECIMAL(18,2) NOT NULL,
    transaction_type ENUM('DEBIT','CREDIT') NOT NULL,
    merchant_id BIGINT,
    category_id BIGINT,
    description VARCHAR(500),
    source_staging_id BIGINT,
    is_manual_override BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id),
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (merchant_id) REFERENCES merchants(merchant_id),
    FOREIGN KEY (category_id) REFERENCES categories(category_id),
    FOREIGN KEY (source_staging_id) REFERENCES ai_transactions_staging(staging_transaction_id)
);

#=====================================NET WORTH TABLES=============================================
#======================================1. ASSETS TABLE============================================
/* Purpose: Store user assets */
CREATE TABLE assets(
    asset_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    asset_name VARCHAR(100) NOT NULL,
    asset_value DECIMAL(15,2) NOT NULL,
    as_of_date DATE NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

#======================================2. LIABILITIES TABLE============================================
/* Purpose: Store user liabilities */
CREATE TABLE liabilities(
    liability_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    liability_name VARCHAR(100) NOT NULL,
    liability_amount DECIMAL(15,2) NOT NULL,
    as_of_date DATE NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

#=====================================AI ASSISTANT MODULES=============================================
#=====================================1. AI CHAT SESSION TABLE=============================================
/* Purpose: store AI chat session*/
CREATE TABLE ai_chat_sessions(
    session_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

#=====================================2. AI CHAT MSG TABLE=============================================
/* Purpose: store AI chat messages*/
CREATE TABLE ai_chat_messages(
    message_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    session_id BIGINT NOT NULL,
    sender ENUM('USER', 'AI') NOT NULL,
    message_text LONGTEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES ai_chat_sessions(session_id) ON DELETE CASCADE
);

#=====================================3. AI MONTHLY SUMMARY TABLE=============================================
/* Purpose: store AI monthly summaries*/
CREATE TABLE ai_monthly_summaries(
    summary_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    summary_month CHAR(7) NOT NULL, 
    summary_text LONGTEXT NOT NULL,
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

