### 

### This project provides a simplified, educational demonstration of core concepts used in commercial End User Monitoring (EUM) solutions, such as capturing user interaction events (here, mouse dwell time on UI elements) and reporting metrics to a backend service.

### 

### While real-world EUM agents involve far more complexity—including performance timing, error tracking, session correlation, data batching, security, and integration with backend APM systems—this example illustrates a fundamental part of their workflow in an accessible way.Info





### NOTE





This example is safe by design and does NOT perform any malicious or unauthorized actions, such as:



Session stealing



Cross-origin script injection



Phishing or credential harvesting



The data collected is limited to anonymous interaction metrics (mouse hover times per square) and does not include sensitive user information.



Cross-Origin Resource Sharing (CORS) is properly configured on the backend to allow only trusted origins to send data, preventing misuse.



The example code is intended solely for learning and demonstration, not for production use or deployment in untrusted environments.


### Note

on a related subject there was a exciting pure JS capable of rendering MRTD payload in canvas (i guess). Can Pure Perl emit mrtg formatted compact data files?
