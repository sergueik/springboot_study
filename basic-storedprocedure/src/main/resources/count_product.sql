DELIMITER $$

USE `training_sp`$$

DROP FUNCTION IF EXISTS `count_product`$$

CREATE DEFINER=`root`@`localhost` FUNCTION `count_product`() RETURNS BIGINT(20)
BEGIN
	DECLARE v_count BIGINT DEFAULT 0;
    
	SELECT  COUNT(1) INTO v_count FROM product;
	RETURN v_count;
    END$$

DELIMITER ;