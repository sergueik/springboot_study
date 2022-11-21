use cardb;

DROP TABLE IF EXISTS carinfo;

CREATE TABLE carinfo (
   id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
   yearofmanufacture INT NOT NULL,
   model VARCHAR(64) NOT NULL,
   make VARCHAR(64) NOT NULL,
   suggestedretailprice FLOAT NOT NULL,
   fullprice FLOAT NOT NULL,
   rebateamount FLOAT NOT NULL,
   createdate DATETIME NOT NULL,
   updatedate DATETIME NOT NULL
);
