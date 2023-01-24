function fn() {
  // configurations.
  var config = {
    
  };
 

  karate.configure('driver', { type: 'chrome', executable: 'C:/Program Files/Google/Chrome/Application/chrome.exe', headless: true  } );
  return config;
}
