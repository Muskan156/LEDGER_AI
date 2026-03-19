#=====================================DATABASE=================================================
CREATE DATABASE ledger_db;
USE ledger_db;
CREATE TABLE professions (
    profession_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    profession_name VARCHAR(100) NOT NULL UNIQUE,
    is_business BOOLEAN NOT NULL DEFAULT FALSE,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE users (
    user_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    profession_id BIGINT NULL,
    email VARCHAR(150) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL,
    status ENUM('ACTIVE','INACTIVE','SUSPENDED') NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP NULL,
    FOREIGN KEY (profession_id) REFERENCES professions(profession_id)
);
INSERT INTO users (email,password_hash) VALUES('muskanshaikh5857@gmail.com','Pass@156');
CREATE TABLE account_groups (
    account_group_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    group_name VARCHAR(100) NOT NULL,
    parent_group_id BIGINT NULL,
    balance_nature ENUM('DEBIT','CREDIT') NOT NULL,
    is_profit_loss BOOLEAN NOT NULL DEFAULT FALSE,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by BIGINT NOT NULL,
    updated_at TIMESTAMP NULL ON UPDATE CURRENT_TIMESTAMP,
    updated_by BIGINT NULL,
    FOREIGN KEY (parent_group_id) REFERENCES account_groups(account_group_id)
);

CREATE TABLE user_sessions(
    session_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    token VARCHAR(255) NOT NULL UNIQUE,
    expires_at TIMESTAMP NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);
CREATE TABLE accounts (
    account_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    account_group_id BIGINT NOT NULL,
    account_name VARCHAR(150) NOT NULL,
    balance_nature ENUM('DEBIT','CREDIT') NOT NULL,
    is_profit_loss BOOLEAN NOT NULL DEFAULT FALSE,
    is_system_generated BOOLEAN NOT NULL DEFAULT TRUE,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id),
    FOREIGN KEY (account_group_id) REFERENCES account_groups(account_group_id)
);

CREATE TABLE profession_account_templates (
    template_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    profession_id BIGINT NOT NULL,
    account_name VARCHAR(150) NOT NULL,
    account_group_id BIGINT NOT NULL,
    balance_nature ENUM('DEBIT','CREDIT') NOT NULL,
    is_profit_loss BOOLEAN NOT NULL DEFAULT FALSE,
    is_default BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (profession_id) REFERENCES professions(profession_id),
    FOREIGN KEY (account_group_id) REFERENCES account_groups(account_group_id)
);

CREATE TABLE profession_category_templates (
    template_category_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    profession_id BIGINT NOT NULL,
    category_name VARCHAR(150) NOT NULL,
    linked_account_template_id BIGINT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (profession_id) REFERENCES professions(profession_id),
    FOREIGN KEY (linked_account_template_id) REFERENCES profession_account_templates(template_id)
);

CREATE TABLE documents (
    document_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    statement_id BIGINT NULL,
    file_name VARCHAR(255) NOT NULL,
    file_path VARCHAR(500) NOT NULL,
    is_password_protected BOOLEAN NOT NULL DEFAULT FALSE,
    transaction_parsed_type ENUM('CODE','LLM') NULL,
    parser_version VARCHAR(50) NULL,
    status ENUM('UPLOADED','PASSWORD_REQUIRED','EXTRACTING_TEXT','IDENTIFYING_FORMAT',
	'PARSING_TRANSACTIONS','AWAITING_REVIEW','CATEGORIZING','POSTED','FAILED') NOT NULL DEFAULT 'UPLOADED',
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    processing_started_at TIMESTAMP NULL,
    processing_completed_at TIMESTAMP NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
    FOREIGN KEY (statement_id) REFERENCES statement_categories(statement_id) ON DELETE SET NULL
);
ALTER TABLE documents 
ADD account_id BIGINT NULL,
ADD account_match_confidence DECIMAL(5,2) NULL,
ADD FOREIGN KEY (account_id) REFERENCES accounts(account_id);
ALTER TABLE documents
MODIFY COLUMN status ENUM(
    'UPLOADED',
    'PASSWORD_REQUIRED',
    'EXTRACTING_TEXT',
    'IDENTIFYING_FORMAT',
    'PARSING_TRANSACTIONS',
    'AWAITING_REVIEW',
    'CATEGORIZING',
    'POSTED',
    'APPROVE',
    'FAILED'
) NOT NULL DEFAULT 'UPLOADED';

CREATE TABLE account_identifiers (
    identifier_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    account_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    institution_name VARCHAR(150),
    account_number_masked VARCHAR(30),
    account_number_last4 VARCHAR(4),
    ifsc_code VARCHAR(20),
    card_last4 VARCHAR(4),
    loan_account_no VARCHAR(50),
    wallet_id VARCHAR(50),
    confidence_score DECIMAL(5,2) DEFAULT 100.00,
    is_primary BOOLEAN DEFAULT FALSE,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (user_id) REFERENCES users(user_id)
);
 
CREATE TABLE document_account_match_log (
    match_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    detected_institution VARCHAR(150),
    detected_account_last4 VARCHAR(4),
    matched_account_id BIGINT NULL,
    confidence_score DECIMAL(5,2),
    match_status ENUM('AUTO_ASSIGNED','USER_CONFIRMED','MANUAL_SELECTED','FAILED'),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (document_id) REFERENCES documents(document_id),
    FOREIGN KEY (matched_account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (user_id) REFERENCES users(user_id)
);


CREATE TABLE document_password (
    document_id BIGINT PRIMARY KEY,
    encrypted_password VARCHAR(255) NOT NULL,
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

CREATE TABLE document_upload_audit(
    audit_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    status ENUM('UPLOADED','PROCESSING','FAILED','COMPLETED') NOT NULL,
    error_message VARCHAR(500),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

CREATE TABLE document_text_extractions (
    text_extraction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    extraction_method ENUM('PDF_TEXT','OCR','HYBRID') NOT NULL DEFAULT 'PDF_TEXT',
    extracted_text LONGTEXT NOT NULL,
    extraction_status ENUM('SUCCESS','FAILED') NOT NULL DEFAULT 'SUCCESS',
    error_message VARCHAR(500),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);

CREATE TABLE statement_categories (
    statement_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    statement_type VARCHAR(50) NOT NULL,
    format_name VARCHAR(150) NOT NULL,
    institution_name VARCHAR(100) NOT NULL,
    ifsc_code VARCHAR(20) NULL,
    statement_identifier JSON NOT NULL,
    extraction_logic LONGTEXT NOT NULL,
    match_threshold DECIMAL(5,2) NOT NULL DEFAULT 65.00,
    logic_version INT NOT NULL DEFAULT 1,
    status ENUM('ACTIVE','UNDER_REVIEW','DISABLED','EXPERIMENTAL') NOT NULL DEFAULT 'UNDER_REVIEW',
    success_rate DECIMAL(5,2) NULL,
    last_verified_at TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

CREATE TABLE ai_transactions_staging (
    staging_transaction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    document_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    transaction_json JSON NOT NULL,
    parser_type ENUM('LLM','CODE') NOT NULL,
    overall_confidence DECIMAL(5,2) NOT NULL,
    review_status ENUM('PENDING','PARTIALLY_APPROVED','APPROVED','REJECTED') NOT NULL DEFAULT 'PENDING',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id),
    FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE
);
ALTER TABLE ai_transactions_staging
DROP COLUMN review_status;
CREATE TABLE transaction_reviews (
    review_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    staging_transaction_id BIGINT NOT NULL,
    reviewer_user_id BIGINT NOT NULL,
    review_status ENUM('APPROVED','REJECTED','NEEDS_CLARIFICATION') NOT NULL,
    review_notes VARCHAR(500),
    reviewed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (staging_transaction_id) REFERENCES ai_transactions_staging(staging_transaction_id) ON DELETE CASCADE,
    FOREIGN KEY (reviewer_user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

CREATE TABLE transaction_overrides (
    override_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    staging_transaction_id BIGINT NOT NULL,
    field_name VARCHAR(50) NOT NULL,
    ai_value VARCHAR(255),
    user_value VARCHAR(255) NOT NULL,
    overridden_by BIGINT NOT NULL,
    overridden_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (staging_transaction_id) REFERENCES ai_transactions_staging(staging_transaction_id) ON DELETE CASCADE,
    FOREIGN KEY (overridden_by) REFERENCES users(user_id) ON DELETE CASCADE
);

CREATE TABLE uncategorized_transactions (
    uncategorized_transaction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    document_id BIGINT NOT NULL,
    statement_id BIGINT NOT NULL,
    staging_transaction_id BIGINT NOT NULL,
    txn_date VARCHAR(50),
    debit DECIMAL(18,2),
    credit DECIMAL(18,2),
    balance DECIMAL(18,2),
    description VARCHAR(500),
    confidence DECIMAL(4,2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (document_id) REFERENCES documents(document_id),
    FOREIGN KEY (statement_id) REFERENCES statement_categories(statement_id),
	FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE,
    FOREIGN KEY (staging_transaction_id) REFERENCES ai_transactions_staging(staging_transaction_id) ON DELETE CASCADE
);

CREATE TABLE entities (
    entity_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    entity_name VARCHAR(255) NOT NULL,
    entity_type ENUM('MERCHANT','EMPLOYER','BANK','WALLET','TRANSFER','OTHER') NOT NULL DEFAULT 'MERCHANT',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (user_id, entity_name),
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

CREATE TABLE entity_aliases (
    alias_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    entity_id BIGINT NOT NULL,
    user_id BIGINT NOT NULL,
    alias_text VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (user_id, alias_text),
    FOREIGN KEY (entity_id) REFERENCES entities(entity_id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);

CREATE TABLE categories (
    category_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    category_name VARCHAR(100) NOT NULL,
    category_type ENUM('INCOME','EXPENSE','TRANSFER') NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);


CREATE TABLE transactions (
    transaction_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    account_id BIGINT NOT NULL,
    document_id BIGINT NOT NULL,
    transaction_date DATE NOT NULL,
    description VARCHAR(500),
    amount DECIMAL(18,2) NOT NULL,
    transaction_type ENUM('DEBIT','CREDIT') NOT NULL,
    entity_id BIGINT NULL,
    category_id BIGINT NULL,
    categorised_by ENUM('DB','LLM','ML') NOT NULL,
    confidence_score DECIMAL(5,2) NOT NULL,
    posting_status ENUM('DRAFT','POSTED','REVERSED') DEFAULT 'DRAFT',
    attention_level ENUM('LOW','MEDIUM','HIGH') NOT NULL DEFAULT 'LOW',
    review_status ENUM('PENDING','APPROVED','REJECTED') NOT NULL DEFAULT 'PENDING',
    source_staging_id BIGINT NULL,
    is_manual_override BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id),
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    FOREIGN KEY (entity_id) REFERENCES entities(entity_id),
    FOREIGN KEY (category_id) REFERENCES categories(category_id),
    FOREIGN KEY (document_id) REFERENCES documents(document_id),
    FOREIGN KEY (source_staging_id) REFERENCES ai_transactions_staging(staging_transaction_id)
);

CREATE TABLE ledger_entries (
    ledger_entry_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    transaction_id BIGINT NOT NULL,
    account_id BIGINT NOT NULL,
    debit_amount DECIMAL(18,2) NOT NULL DEFAULT 0.00,
    credit_amount DECIMAL(18,2) NOT NULL DEFAULT 0.00,
    entry_date DATE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (transaction_id) REFERENCES transactions(transaction_id) ON DELETE CASCADE,
    FOREIGN KEY (account_id) REFERENCES accounts(account_id)
);

#=====================================NET WORTH TABLES=============================================
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

CREATE TABLE ai_chat_messages(
    message_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    session_id BIGINT NOT NULL,
    sender ENUM('USER', 'AI') NOT NULL,
    message_text LONGTEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (session_id) REFERENCES ai_chat_sessions(session_id) ON DELETE CASCADE
);


CREATE TABLE ai_monthly_summaries(
    summary_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    summary_month CHAR(7) NOT NULL, 
    summary_text LONGTEXT NOT NULL,
    generated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);



