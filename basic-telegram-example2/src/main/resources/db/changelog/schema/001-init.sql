CREATE TABLE usr (
                     user_id integer NOT NULL,
                     name varchar,
                     oil_coin integer,
                     gold integer,
                     e_coin integer,
                     e_crypt integer,
                     references_url varchar,
                     user_role varchar,
                     positions varchar,
                     balls_1 decimal,
                     balls_2 decimal,
                     refer_id integer,
                     reg_date varchar,
                     task_completed integer,
                     qiwi varchar,
                     daily_bonus boolean,
                     count_referals integer,
                     joined boolean,
                     oil_producted integer,
                     electric_producted integer,
                     oil_product_time integer,
                     electric_product_time integer,
                     CONSTRAINT pk_user PRIMARY KEY (user_id)
);

CREATE TABLE payment (
                         id serial NOT NULL,
                         user_id integer,
                         sum integer,
                         time varchar,
                         p_time varchar,
                         success boolean,
                         CONSTRAINT pk_payment PRIMARY KEY (id)
);

CREATE TABLE channel (
            id varchar,
            url varchar,
            CONSTRAINT pk_channel PRIMARY KEY (id)
);

CREATE TABLE actions (
                         actions_id serial NOT NULL,
                         name_company varchar,
                         user_id integer,
                         company_id integer,
                         quantity integer,
                         type varchar,
                         CONSTRAINT pk_actions PRIMARY KEY (actions_id)
);

CREATE TABLE oil_pump (
                         pump_id serial NOT NULL,
                         name varchar,
                         level integer,
                         price integer,
                         production integer,
                         user_id integer,
                         producted integer,
                         CONSTRAINT pk_pump PRIMARY KEY (pump_id)
);
CREATE TABLE powerstation (
                          power_id serial NOT NULL,
                          name varchar,
                          level integer,
                          price integer,
                          production integer,
                          user_id integer,
                          producted integer,
                          CONSTRAINT pk_power PRIMARY KEY (power_id)
);
INSERT INTO powerstation (power_id, name, level, price, production, user_id, producted)
 VALUES (1,'Солнечная электростанция',1,100,16,0,0),
        (2, 'Ветряная электростанция',2,1000,184,0,0),
        (3, 'Тепловая электростанция',3,6000,1249,0,0),
        (4, 'Гидроэлектростанция',4,18000,4463,0,0),
        (5, 'Атомная электростанция',5,45000,13020,0,0),
        (6, 'Термоядерная электростанция',6,90000,31250,0,0);

INSERT INTO oil_pump (pump_id, name, level, price, production, user_id, producted)
VALUES (1,'Деревянный ручной насос',1,100,16,0,0),
       (2, 'Металлический насос',2,1000,184,0,0),
       (3, 'Фабричный насос',3,6000,1249,0,0),
       (4, 'Профессиональный насос',4,18000,4463,0,0),
       (5, 'Насос с ионным двигателем',5,45000,13020,0,0),
       (6, 'Насос на квантовой тяге',6,90000,31250,0,0);