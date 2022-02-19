package example.controllers;


import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import example.model.User;
import example.service.UserService;

import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

@Controller
@Slf4j
@RequiredArgsConstructor
@RequestMapping("/payment")
public class CapitalistRestController {

    private final UserService userService;
    int merchantId = 100; // id магазина

    @RequestMapping(value = "/getpay", method = POST, consumes = APPLICATION_JSON_VALUE, produces = APPLICATION_JSON_VALUE)
    @ResponseBody
    public String getPay(@RequestBody JsonDTO jsonDTO) {
        if (jsonDTO == null){
            return "NOT";
        }

        log.info("Операция {}", jsonDTO.toString());
        String type = jsonDTO.getUs_type();
        User user = userService.getOrCreate(jsonDTO.getMerchant_order_id());
        int sum = jsonDTO.getAmount();
        String label = jsonDTO.getSign();

        if (!getMd5(sum, user.getUserId()).equalsIgnoreCase(label)){
            return "NOT";
        }

        if ("ecoin".equalsIgnoreCase(type)){
            updateECoin(user, sum);
        } else if ("oilcoin".equalsIgnoreCase(type)){
            updateOilCoin(user, sum);
        }

        log.info("Операция {} успешна", jsonDTO);
        return "OK";
    }

    private void updateECoin(User user, int sum){
        int eCoin = sum / 3;
        double ballsOne = 0;
        User refer = null;

        user.setECoin(user.getECoin() + eCoin);

        if (user.getReferId() != 0) {
            refer = userService.getOrCreate(user.getReferId());
            refer.setECoin(refer.getECoin() + (eCoin/100)*30);
        }

        if (eCoin >= 200){
            while (eCoin >= 200){
                ballsOne++;
                eCoin-=200;
            }
        }

        if (user.getReferId() != 0) {
            refer = userService.getOrCreate(user.getReferId());
            refer.setBallsOne(refer.getBallsOne() + (ballsOne/100)*30);
        }

        if (refer != null){
            userService.update(refer);
        }

        user.setBallsOne(user.getBallsOne() + ballsOne);
        userService.update(user);
        log.info("юзеру {} начислено ecoin {} и балловЖелтых {} за {} рублей", user.getUserId(), eCoin, ballsOne, sum);
    }

    private void updateOilCoin(User user, int sum){
        int oilCoin = (sum / 6) * 10;
        double ballsTwo = 0;
        User refer = null;

        user.setOilCoin(user.getOilCoin() + oilCoin);

        if (user.getReferId() != 0) {
            refer = userService.getOrCreate(user.getReferId());
            refer.setOilCoin(refer.getOilCoin() + (oilCoin/100)*30);
        }

        if (oilCoin >= 200){
            while (oilCoin >= 200){
                ballsTwo++;
                oilCoin-=200;
            }
        }

        if (user.getReferId() != 0) {
            refer = userService.getOrCreate(user.getReferId());
            refer.setBallsTwo(refer.getBallsTwo() + (ballsTwo/100)*30);

        }

        if (refer != null){
            userService.update(refer);
        }

        user.setBallsTwo(user.getBallsTwo() + ballsTwo);
        userService.update(user);
        log.info("юзеру {} начислено oilcoin {} и балловСиних {} за {} рублей", user.getUserId(), oilCoin, ballsTwo, sum);
    }

    // обработка подписи freeKassa
    @SneakyThrows
    private String getMd5(int sum, int userId){
        String secretWord = "word";
        String label = merchantId + ":" + sum +":" + secretWord + ":" + userId;
        return DigestUtils.md5Hex(label);
    }
}