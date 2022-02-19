package example.controllers;

import lombok.Getter;
import lombok.Setter;

import java.util.Objects;

@Getter
@Setter
class JsonDTO {

    int merchant_id;	//ID Вашего магазина
    int amount; // Сумма заказа
    int intid;	// Номер операции Free-Kassa
    int merchant_order_id;	//Ваш номер заказа -- в моем случае userId
    String p_email;	// Email плательщика
    String p_phone;	// Телефон плательщика (если указан)
    int cur_id;	// ID электронной валюты, который был оплачен заказ (список валют)
    String sign; // Подпись (методика формирования подписи в данных оповещения)
    String us_type;	// Дополнительные параметры с префиксом us_, переданные в форму оплаты(тут тип валюты)

    @Override
    public String toString() {
        return "JsonDTO{" +
                "merchant_id=" + merchant_id +
                ", amount=" + amount +
                ", intid=" + intid +
                ", merchant_order_id=" + merchant_order_id +
                ", p_email='" + p_email + '\'' +
                ", p_phone='" + p_phone + '\'' +
                ", cur_id=" + cur_id +
                ", sign='" + sign + '\'' +
                ", us_type='" + us_type + '\'' +
                '}';
    }
}