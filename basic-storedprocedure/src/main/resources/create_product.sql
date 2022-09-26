DELIMITER $$

USE `test`$$

DROP PROCEDURE IF EXISTS `create_product`$$

CREATE DEFINER=`root`@`localhost` PROCEDURE `create_product`(id VARCHAR(255), p_code VARCHAR(255),p_name VARCHAR(255),weight BIGINT)
BEGIN
	
	INSERT INTO product(id, CODE,NAME,weight) VALUES(id,p_code,p_name,weight);
    END$$

DELIMITER ;