CREATE DATABASE application_db;

CREATE TABLE clan (
clan_id serial PRIMARY KEY,
clan_name varchar(50) NOT NULL,
gold bigint NOT NULL
);

CREATE TABLE player (
player_id serial PRIMARY KEY,
name varchar(50) NOT NULL,
clan_id integer,
FOREIGN KEY(clan_id) REFERENCES clan (clan_id)
);

CREATE TABLE transactions (
trans_id serial PRIMARY KEY,
player_id integer NOT NULL,
clan_id integer NOT NULL,
action varchar(200) NOT NULL,
money bigint NOT NULL,
date timestamp DEFAULT CURRENT_TIMESTAMP(3),
FOREIGN KEY(player_id) REFERENCES player (player_id),
FOREIGN KEY(clan_id) REFERENCES clan (clan_id)
);
