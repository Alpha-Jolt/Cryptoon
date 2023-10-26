--User Infogrpahics
Create Table User_Infographics (ID_Number Serial Primary Key, First_Name VARCHAR(50) Not Null, Last_Name VARCHAR(50), E_Mail_ID VARCHAR(50) Not Null, Phone_Number BIGINT Not Null, Username VARCHAR(50) UNIQUE Not Null);


--Wallet
Create Table Wallet (Wallet_public_key VARCHAR(255), Username VARCHAR(50));


--Login Details--
Create Table Login_Details (ID_Number INT, Username VARCHAR(50) Not Null, Password_salt TEXT Not Null, Password_hash TEXT Not Null);

Alter Table Login_Details
ADD Constraint fk_Login_Details_Login
	Foreign Key (ID_Number)
	References User_Infographics (ID_Number);

--Extenstion
Create EXTENSION if Not exists pgcrypto;

DO $$
--Declaration
DECLARE user_salt TEXT;
DECLARE user_password TEXT;
DECLARE user_hash TEXT;
BEGIN
--Salting and Hashing
user_salt := gen_salt ('bf', 8);
user_password := 'user_provided_password' || user_salt;
user_hash := crypt(user_password, user_salt);
INSERT into Login_Details (Username, Password_salt, Password_hash) Values ('new_user', user_salt, user_hash);
END $$;


--KYC
Create Table Know_Your_Customer (KYC_ID Serial Primary Key, ID_Number INT Not Null, National_ID_Path VARCHAR(255) Not Null, National_ID_Number VARCHAR(255) Not Null, Selfie BYTEA Not Null, Verification_Status VARCHAR(50) Not Null, Verification_Timestamp TIMESTAMP Not Null,
	--Auditing Fields--
	Kyc_created TIMESTAMP Default NOW() Not Null, Re_verification TIMESTAMP Default Updation() Not Null, Updated_at TIMESTAMP NOW() Not Null,);

Alter Table KnowYour_Customer
ADD Constraint fk_Know_Your_Customer_KYC
	Foreign Key (ID_Number)
	References User_Infographics (ID_Number);

--KYC Audit
Create Table KYC_Audit (Audit_ID Serial Primary Key, Updated_at TIMESTAMP, Original_KYC_ID INT, Previous_Values JSONB, Current_Values JSONB);


--Functions & Triggers

--Updation
Create Or Replace FUNCTION Updation()
Returns TIMESTAMP As
$$
BEGIN
	Return NOW()+INTERVAL '1 Year';
END;
$$
LANGUAGE plpgsql;

--Insertation
Create or Replace FUNCTION Audit_KYC()
Returns TRIGGER As $$
BEGIN
	INSERT into KYC_Audit (Updated_at, Original_KYC_ID, Previous_Values, Current_Values) Values (NEW.KYC_ID, (SELECT to_jsonb(OLD)), (SELECT to_jsonb(OLD)), (SELECT to_jsonb(NEW)));
	Return NEW
END;
$$
LANGUAGE plpgsql;

--Trigger
Create TRIGGER Audit_Trigger
Before UPDATE on Know_Your_Customer
For Each ROW
When (NEW.Re_verification is DISTINCT from OLD.Re-verification)
EXECUTE FUNCTION Audit_KYC();

--Table Alteration
Alter Table KYC_Audit
ADD Constraint fk_KYC_Audit_KYC
	Foreign Key (Original_KYC_ID)
	References Know_Your_Customer(KYC_ID);

--Relational Database for YOLO
