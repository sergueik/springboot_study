package ru.habrahabr;

import javafx.embed.swing.JFXPanel;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
// NOTE: compilation problems when upgraded the org.springframework.boot.spring-boot-starter-parent
// from 1.2.5.RELEASE to 1.5.4.RELEASE
// package org.springframework.boot.test does not exist
// import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
// @SpringApplicationConfiguration(classes = Application.class)
public class ApplicationTests {

	@BeforeClass
	public static void bootstrapJavaFx() {
		// implicitly initializes JavaFX Subsystem
		// see
		// http://stackoverflow.com/questions/14025718/javafx-toolkit-not-initialized-when-trying-to-play-an-mp3-file-through-mediap
		new JFXPanel();
	}

	@Test
	public void contextLoads() {
	}

}
