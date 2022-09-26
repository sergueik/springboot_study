package example.api;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import example.domain.Product;
import example.utils.DatabaseUtils;
import example.utils.JdbcTemplateUtils;

import java.sql.Types;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

/**
 * Created by krisna putra on 10/30/2017.
 */
@RestController
@RequestMapping("api/product-sp")
public class ProductApi {
    private DatabaseUtils utils;
    @Autowired
    public void setUtils(DatabaseUtils utils) {
        this.utils = utils;
    }
    private JdbcTemplateUtils jdbcUtils;

    @Autowired
    public void setJdbcUtils(JdbcTemplateUtils jdbcUtils){
        this.jdbcUtils=jdbcUtils;
    }

    @PostMapping("/sp-create-product")
    public String callProcedure(@RequestBody Product product){
        Object[] params=new Object[]{
                UUID.randomUUID().toString(),
                product.getCode(),
                product.getName(),
                product.getWeight()
        };
        utils.callStoredProcedure("create_product(?,?,?,?)",params);
        return "success";
    }
    @GetMapping("/sp-count-product")
    public Long callFunctionCountProduct(){
        Long count=(Long) utils.callStoredFunction(Types.BIGINT, "count_product()",null);
        return count;
    }

    @PostMapping("/sp-create-product-jdbc")
    public String callProcedureJdbc(@RequestBody Product product){
        Map<String,Object> params=new HashMap<>();
        params.put("id",UUID.randomUUID().toString());
        params.put("p_code",product.getCode());
        params.put("p_name",product.getName());
        params.put("weight",product.getWeight());
        jdbcUtils.callStoreProcedure("create_product",params);
        return "success";
    }
    @GetMapping("/sp-count-product-jdbc")
    public Long callFunctionCountProductJdbc(){
        Long count=(Long) jdbcUtils.callStoredFunction( "count_product",null,Long.class);
        return count;
    }
}
