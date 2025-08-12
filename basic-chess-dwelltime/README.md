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

### Application  Architecture

The cluster of applications run on docker consists of

  * `app1`:** Static content server (plain Apache2) serving the subject pages (e.g., chessboard UI) — minimal footprint, illustrating the client side EUM without applcation integration.
   *   `app2`: EUM metric processoras EUM metric collector configured with full CORS access from subject pages and performing statistical analysis and wrappping arriving data (JSON POST handling) illustrated with Mojolicious backend serving .
  * `app3` (optional, WIP): another subject app with alternative EUM processing including applicsation integration  (WIP) represented by an Nginx container for alternate data processing, proxying, or load balancing tasks.

### Note

on a related subject there was a exciting pure JS capable of rendering MRTD payload in canvas (i guess). Can Pure Perl emit mrtg formatted compact data files?

![example page](screenshots/png.png)

```text
app2  | [2025-08-12 22:54:29.57301] [6] [info] Received payload: "{\"b3\":4.97,\"a2\":17.38,\"b2\":10.45,\"c3\":11.05,\"c2\":4.81,\"a1\":0.54,\"b1\":18.34,\"a3\":0.24,\"c1\":0.1}"
```
