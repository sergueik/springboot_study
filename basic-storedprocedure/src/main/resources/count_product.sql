SET GLOBAL log_bin_trust_function_creators = 1;
DELIMITER $$

USE `test`$$

DROP FUNCTION IF EXISTS `count_product`$$

CREATE DEFINER=`root`@`localhost` FUNCTION `count_product`() RETURNS BIGINT(20)
BEGIN
	DECLARE v_count BIGINT DEFAULT 0;
    
	SELECT  COUNT(1) INTO v_count FROM product;
	RETURN v_count;
    END$$

DELIMITER ;