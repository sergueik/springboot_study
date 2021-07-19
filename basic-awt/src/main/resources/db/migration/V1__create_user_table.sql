CREATE TABLE IF NOT EXISTS user_table (
  id         BIGSERIAL      PRIMARY KEY,
  name       VARCHAR(255)   NOT NULL,
  roles      VARCHAR(255)   NOT NULL,
  password   VARCHAR(255)   NOT NULL,
  email      VARCHAR(255)   NOT NULL UNIQUE
);
