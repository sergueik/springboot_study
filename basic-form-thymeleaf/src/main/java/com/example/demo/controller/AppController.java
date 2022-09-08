package com.example.demo.controller;

import com.example.demo.model.Category;
import com.example.demo.model.Product;
import com.example.demo.service.CategoryService;
import com.example.demo.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.util.List;

@Controller
@RequiredArgsConstructor
public class AppController {

	private final ProductService service;
	private final CategoryService categoryService;

	// поиск
	@RequestMapping("/")
	public String viewHomePage(Model model, @Param("keyword") String keyword) {
		List<Product> listProducts = service.listAll(keyword);
		List<Category> categoryList = categoryService.getAll();

		model.addAttribute("listProducts", listProducts);
		model.addAttribute("categoryList", categoryList);
		model.addAttribute("keyword", keyword);

		return "index";
	}

	// добавить продукт
	@RequestMapping("/new_product")
	public String showNewProductForm(Model model) {
		Product product = new Product();
		List<Category> category = categoryService.getAll();
		model.addAttribute("categories", category);
		model.addAttribute("product", product);

		return "new_product";
	}


	@RequestMapping(value = "/save", method = RequestMethod.POST)
	public String saveProduct(@ModelAttribute("product") Product product) {

		if (product != null){
			service.save(product);

			return "redirect:/";
		}else {
			return "404";
		}

	}

	@RequestMapping("/edit/{id}")
	public ModelAndView showEditProductForm(@PathVariable(name = "id") Long id, Category category) {
		ModelAndView mav = new ModelAndView("edit_product");
		
		Product product = service.get(id);
		mav.addObject("product", product);
		mav.addObject("category", category);
		return mav;
	}	
	
	@RequestMapping("/delete/{id}")
	public String deleteProduct(@PathVariable(name = "id") Long id) {
		service.delete(id);
		
		return "redirect:/";
	}


//	Котегория:

	// новая форма категории
	@RequestMapping("/new_category")
	public String showNewCategoryForm(Model model) {
		Category category = new Category();
		model.addAttribute("category", category);

		return "new_category";
	}

	// сохранить категорию в базу данных
	@PostMapping(value = "/saveCategory")
	public String saveCategory(@ModelAttribute("category") Category category) {
		if (category != null){
			categoryService.save(category);
			return "redirect:/";
		}else {
			return "404";
		}

	}

	@RequestMapping("/editCategory/{id}")
	public ModelAndView showEditCategoryForm(@PathVariable(name = "id") Long id) {
		ModelAndView mav = new ModelAndView("edit_category");

		Category category = categoryService.get(id);
		mav.addObject("category", category);
		return mav;
	}

	@RequestMapping("/deleteCategory/{id}")
	public String deleteCategory(@PathVariable(name = "id") Long id) {
		categoryService.delete(id);

		return "redirect:/";
	}
}
