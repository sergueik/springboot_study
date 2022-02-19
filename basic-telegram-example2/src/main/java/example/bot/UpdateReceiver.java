package example.bot;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.stereotype.Component;
import org.telegram.telegrambots.meta.api.methods.AnswerCallbackQuery;
import org.telegram.telegrambots.meta.api.methods.send.SendMessage;
import org.telegram.telegrambots.meta.api.objects.Update;

import example.bot.handler.CallbackHandler;
import example.bot.handler.StartHandler;
import example.gameEntities.*;

import java.util.List;


@Component
@Slf4j
@ComponentScan(basePackages = "application.yaml")
public class UpdateReceiver {

    private final StartHandler startHandler;
    private final CallbackHandler callbackHandler;
    private final Bank bank;
    private final MyCompany myCompany;
    private final Market market;
    private final Addition addition;
    private final Games games;
    private final Actions actions;

    @Autowired
    public UpdateReceiver(StartHandler startHandler, CallbackHandler callbackHandler, Bank bank, MyCompany myCompany, Market market, Addition addition, Games games, Actions actions) {
        this.startHandler = startHandler;
        this.callbackHandler = callbackHandler;
        this.bank = bank;
        this.myCompany = myCompany;
        this.market = market;
        this.addition = addition;
        this.games = games;
        this.actions = actions;
    }

    public SendMessage mainMenu(Update update){
        return callbackHandler.mainMenu(update);
    }

    public SendMessage main(Update update){
        return callbackHandler.main(update);
    }

    public SendMessage start(Update update){
        return startHandler.start(update);
    }

    public SendMessage callbackStart(Update update){
        return startHandler.callbackStart(update);
    }

    public SendMessage startTwo(Update update){
        return startHandler.stepTwo(update);
    }

    public SendMessage startThree(Update update){
        return startHandler.stepThree(update);
    }

    public SendMessage startFour(Update update){
        return startHandler.stepFour(update);
    }

    public SendMessage startFive(Update update){
        return startHandler.stepFive(update);
    }

    public SendMessage startSix(Update update){
        return startHandler.stepSix(update);
    }

    public SendMessage endTrain(Update update){
        return startHandler.endTrain(update);
    }

    public SendMessage endTrainMenu(Update update){
        return startHandler.endTrainMenu(update);
    }

    public SendMessage actionAndRef(Update update){
        return callbackHandler.actionAndRef(update);
    }

    public SendMessage chat(Update update){
        return callbackHandler.chat(update);
    }

    public SendMessage advert(Update update){
        return callbackHandler.advert(update);
    }

    public SendMessage pay(Update update){
        return bank.pay(update);
    }

    public SendMessage mainCompany(Update update){
        return myCompany.main(update);
    }

    public SendMessage mainMarket(Update update){
        return market.mark(update);
    }

    public boolean marketEnough(int userId, String type){
        return type.equalsIgnoreCase("oil") ? market.enoughOil(userId) : market.enoughElectric(userId);
    }

    public AnswerCallbackQuery alertOil(Update update){
        return callbackHandler.creatAlertCallbackOil(update);
    }

    public AnswerCallbackQuery alertElectric(Update update){
        return callbackHandler.creatAlertCallbackElectric(update);
    }

    public SendMessage oilPump(Update update){
        return myCompany.oilPump(update);
    }

    public SendMessage electric(Update update){
        return myCompany.electric(update);
    }

    public List<SendMessage> buyOilPump(Update update){
        return myCompany.buyOilPump(update);
    }

    public List<SendMessage> buyElectric(Update update){
        return myCompany.buyElectric(update);
    }

    public SendMessage buyOilPumpImpl(Update update, int level){
        return myCompany.buyOilPumpImpl(update, level);
    }

    public SendMessage buyElectricImpl(Update update, int level){
        return myCompany.buyElectricImpl(update, level);
    }

    public AnswerCallbackQuery creatAlertCallbackOilNotMoney(Update update){
        return callbackHandler.creatAlertCallbackOilNotMoney(update);
    }

    public AnswerCallbackQuery creatAlertCallbackReturnTrain(Update update){
        return callbackHandler.creatAlertCallbackReturnTrain(update);
    }

    public AnswerCallbackQuery creatAlertCallbackElectricNotMoney(Update update){
        return callbackHandler.creatAlertCallbackElectricNotMoney(update);
    }

    public SendMessage buyElectricOptom(Update update, int level){
        return myCompany.buyElectricOptom(update, level);
    }

    public SendMessage buyOilPumpOptom(Update update, int level){
        return myCompany.buyOilPumpOptom(update, level);
    }

    public SendMessage buyElectricOptomImpl(Update update, int level, int quantity){
        return myCompany.buyElectricOptomImpl(update, level, quantity);
    }

    public SendMessage buyOilPumpOptomImpl(Update update, int level, int quantity){
        return myCompany.buyOilPumpOptomImpl(update, level, quantity);
    }

    public SendMessage stat(Update update){
        return myCompany.stat(update);
    }

    public List<SendMessage> referals(Update update){
        return myCompany.referals(update);
    }

    public SendMessage changeBalls(Update update){
        return bank.changeBalls(update);
    }

    public SendMessage changeGold(Update update){
        return bank.changeGold(update);
    }

    public SendMessage changeEcrypt(Update update){
        return bank.changeEcrypt(update);
    }

    public SendMessage change(Update update){
        return bank.change(update);
    }

    public SendMessage paymentECoin(Update update) {
        return bank.paymentECoin(update);
    }

    public SendMessage paymentOilCoin(Update update) {
        return bank.paymentOilCoin(update);
    }

    public SendMessage payment(Update update) {
        return bank.payment(update);
    }

    public SendMessage withdraw(Update update){
        return bank.withdraw(update);
    }

    public SendMessage withdrawGold(Update update){
        return bank.withdrawGold(update);
    }

    public SendMessage withdrawECrypt(Update update){
        return bank.withdrawECrypt(update);
    }

    public SendMessage changeEcryptImpl(Update update) { return bank.changeEcryptImpl(update);}

    public SendMessage changeGoldImpl(Update update) { return bank.changeGoldImpl(update);}

    public SendMessage changeBallsImpl(Update update) { return bank.changeBallsImpl(update);}

    public SendMessage withdrawECryptImpl(Update update){
        return bank.withdrawECryptImpl(update);
    }

    public SendMessage withdrawGoldImpl(Update update){
        return bank.withdrawGoldImpl(update);
    }

    public SendMessage electricToBox(Update update){
        return myCompany.electricToBox(update);
    }

    public SendMessage oilToBox(Update update){
        return myCompany.oilToBox(update);
    }

    public SendMessage sellOil(Update update){
        return market.sellOil(update);
    }

    public SendMessage sellElectric(Update update){
        return market.sellElectric(update);
    }

    public SendMessage sellOilImpl(Update update){
        return market.sellOilImpl(update);
    }

    public SendMessage sellElectricImpl(Update update){
        return market.sellElectricImpl(update);
    }

    public SendMessage dailyBonus(Update update){
        return games.dailyBonus(update);
    }

    public SendMessage add(Update update){
        return addition.add(update);
    }

    public SendMessage addName(Update update){
        return addition.addName(update);
    }

    public SendMessage changeName(Update update){
        return addition.changeName(update);
    }

    public SendMessage changeNameImpl(Update update){
        return addition.changeNameImpl(update);
    }

    public SendMessage faq(Update update){
        return addition.faq(update);
    }

    public SendMessage report(Update update){
        return addition.report(update);
    }

    public SendMessage reportImpl(Update update){
        return addition.reportImpl(update);
    }

    public SendMessage reportImplToUser(Update update){
        return addition.reportImplToUser(update);
    }

    public SendMessage help(Update update){
        return addition.help(update);
    }

    public SendMessage answer(Update update){
        return addition.answer(update);
    }

    public List<SendMessage> answerImpl(Update update){
        return addition.answerImpl(update);
    }

    public SendMessage faqBalls(Update update){
        return addition.faqBalls(update);
    }

    public SendMessage faqRefs(Update update){
        return addition.faqRefs(update);
    }

    public SendMessage faqActions(Update update){
        return addition.faqActions(update);
    }

    public SendMessage faqWith(Update update){
        return addition.faqWith(update);
    }

    public SendMessage community(Update update){
        return addition.community(update);
    }

    public SendMessage action(Update update){
        return actions.action(update);
    }

    public SendMessage actions(Update update){
        return actions.actions(update);
    }

    public SendMessage buyAction(Update update){
        return actions.buyAction(update);
    }

    public SendMessage myActions(Update update){
        return actions.myActions(update);
    }

    public AnswerCallbackQuery creatAlertCallbackBuyAction(Update update){
        return callbackHandler.creatAlertCallbackBuyAction(update);
    }

    public SendMessage findCompany(Update update){
        return actions.findCompany(update);
    }

    public SendMessage findCompanyImpl(Update update){
        return actions.findCompanyImpl(update);
    }

    public SendMessage findCompanyCheck(Update update){
        return actions.findCompanyCheck(update);
    }

    public SendMessage myActioner(Update update){
        return actions.myActioner(update);
    }

    public SendMessage myActionerImpl(Update update){
        return actions.myActionerImpl(update);
    }

    public SendMessage topReferals(Update update){
        return addition.topRefs(update);
    }

    public SendMessage task(Update update){
        return myCompany.task(update);
    }
    public SendMessage getBonusForJoin(Update update){
        return games.getBonusForJoin(update);
    }


}
