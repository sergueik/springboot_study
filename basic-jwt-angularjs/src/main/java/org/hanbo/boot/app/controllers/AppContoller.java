package org.hanbo.boot.app.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class AppContoller {
	@RequestMapping(value = "/public/index", method = RequestMethod.GET)
	public ModelAndView index() {
		ModelAndView retVal = new ModelAndView();
		retVal.setViewName("indexPage");
		return retVal;
	}
}