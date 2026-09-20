```cmd
pushd "%LOCALAPPDATA%\Google\Chrome\User Data robocopy.exe /s . "..\User Data".BACKUP
```

  Manually examine data in __Browse Data__


|`url`| `title` |`visit_count` |
|-|-|-|
|https://mytrip.frenchbee.com/booking?lang=fr&search=%7B%22commercialFareFamilies%22%3A%5B%22ECONEW%22%5D%2C%22travelers%22%3A%5B%7B%22passengerTypeCode%22%3A%22ADT%22%7D%2C%7B%22passengerTypeCode%22%3A%22ADT%22%7D%5D%2C%22itineraries%22%3A%5B%7B%22originLocationCode%22%3A%22MIA%22%2C%22destinationLocationCode%22%3A%22%22%2C%22departureDateTime%22%3A%222026-09-12%22%7D%2C%7B%22originLocationCode%22%3A%22%22%2C%22destinationLocationCode%22%3A%22MIA%22%2C%22departureDateTime%22%3A%222026-09-19%22%7D%5D%7D&portalFacts=%5B%7B%22key%22%3A%22countryCode%22%2C%22value%22%3A%22FR%22%7D%5D&_gl=1*162hbjg*_gcl_au*OTgxMTEwMDI5LjE3ODQzNDYwMjM.*_ga*MTUyNzg5NDM1MC4xNzg0MzQ2MDIz*_ga_L4KY30N8F3*czE3ODQzNDYwMjIkbzEkZzEkdDE3ODQzNDYwMjQkajU4JGwwJGg2OTQ1NDk2MTU.*_fplc*UFZtUXdEUlBxVE03T2czb2tVOG12QlM5VDNEJTJCJTJCdk9JaHhNbzR5bWlOSnpTQUlpMWFjakF3S1NkSmRyTVBHQmM2dXN3MW02RjljRklaQ0RHQ2pieFBDWlhleE1XJTJCUjY0UTlmejA3QTVxTkxYaVNPdEZpd2hFZkdUY2JhT3RBJTNEJTNE|Booking|2|
|https://mytrip.frenchbee.com/booking/availability/0|Sélection du vol|5|
|https://mytrip.frenchbee.com/bf/statics/booking/app/13.0.59-bf-booking.2/#|Booking|5
|https://mytrip.frenchbee.com/booking/availability/1|Sélection du vol|4|
|https://mytrip.frenchbee.com/booking/shopping-cart|Panier|13|
|https://mytrip.frenchbee.com/booking/traveler/0|Informations voyageurs|4|
|https://mytrip.frenchbee.com/booking/traveler/1|Informations voyageurs|7|
|https://mytrip.frenchbee.com/booking/seatmap/ST1|Plan de cabine|5|
|https://mytrip.frenchbee.com/booking/seatmap/ST2|Plan de cabine|4|
|https://mytrip.frenchbee.com/booking/services/MEAL|Sélection de repas|2|
|https://mytrip.frenchbee.com/booking?lang=en&search=%7B%22commercialFareFamilies%22%3A%5B%22ECONEW%22%5D%2C%22travelers%22%3A%5B%7B%22passengerTypeCode%22%3A%22ADT%22%7D%2C%7B%22passengerTypeCode%22%3A%22ADT%22%7D%5D%2C%22itineraries%22%3A%5B%7B%22originLocationCode%22%3A%22MIA%22%2C%22destinationLocationCode%22%3A%22ORY%22%2C%22departureDateTime%22%3A%222026-09-10%22%7D%2C%7B%22originLocationCode%22%3A%22ORY%22%2C%22destinationLocationCode%22%3A%22MIA%22%2C%22departureDateTime%22%3A%222026-09-19%22%7D%5D%7D&portalFacts=%5B%7B%22key%22%3A%22countryCode%22%2C%22value%22%3A%22US%22%7D%5D|Sélection du vol|8|
|https://mytrip.frenchbee.com/booking/availability/1|Sélection du vol|4|

...


```sql
SELECT datetime(last_visit_time/1000000-11644473600,'unixepoch','localtime') as last_visit_time, title, url
FROM urls WHERE url LIKE '%frenchbee%' ORDER BY last_visit_time;
```

|`last_visit_time` | `title` | `url` |
|--|--|--|
|2026-07-17 23:40:34|Booking|https://mytrip.frenchbee.com/booking?lang=fr&amp;search=%7B%22commercialFareFamilies%22%3A%5B%22ECONEW%22%5D%2C%22travelers%22%3A%5B%7B%22passengerTypeCode%22%3A%22ADT%22%7D%2C%7B%22passengerTypeCode%22%3A%22ADT%22%7D%5D%2C%22itineraries%22%3A%5B%7B%22originLocationCode%22%3A%22MIA%22%2C%22destinationLocationCode%22%3A%22%22%2C%22departureDateTime%22%3A%222026-09-12%22%7D%2C%7B%22originLocationCode%22%3A%22%22%2C%22destinationLocationCode%22%3A%22MIA%22%2C%22departureDateTime%22%3A%222026-09-19%22%7D%5D%7D&amp;portalFacts=%5B%7B%22key%22%3A%22countryCode%22%2C%22value%22%3A%22FR%22%7D%5D&amp;_gl=1*162hbjg*_gcl_au*OTgxMTEwMDI5LjE3ODQzNDYwMjM.*_ga*MTUyNzg5NDM1MC4xNzg0MzQ2MDIz*_ga_L4KY30N8F3*czE3ODQzNDYwMjIkbzEkZzEkdDE3ODQzNDYwMjQkajU4JGwwJGg2OTQ1NDk2MTU.*_fplc*UFZtUXdEUlBxVE03T2czb2tVOG12QlM5VDNEJTJCJTJCdk9JaHhNbzR5bWlOSnpTQUlpMWFjakF3S1NkSmRyTVBHQmM2dXN3MW02RjljRklaQ0RHQ2pieFBDWlhleE1XJTJCUjY0UTlmejA3QTVxTkxYaVNPdEZpd2hFZkdUY2JhT3RBJTNEJTNE|
|2026-07-17 23:53:35|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&amp;pf=1&amp;ai=DChsSEwjUnpzvqduVAxWrRv8BHSDaCHIYACICCAEQARoCbWQ&amp;co=1&amp;ase=2&amp;gclid=EAIaIQobChMI1J6c76nblQMVq0b_AR0g2ghyEAAYASAAEgJPsvD_BwE&amp;cid=CAASugHkaP7LsesU8fHVhOl7niz8Lp-tLQjRt2kb7TIMWciRw8M2ao9b_paYLpVPsXKGe7PxiUjjdKoyn_P0-VL_mF3Kpy15jFesvKRcxt_CtjqbHKj2O5ytSOygSyT_whWhuSyQ87dNIt9UgGtQfqka9UPWnz426gPyeyHqO1a_x2msE0F_b_gW7XFIqNeC6hJ0CFGCHVg5If1exXkJ2Ih9oeO7_3-SSqwNcUSdPxkJ2yQbUHsucMj-MOt87o4&amp;cce=2&amp;category=acrcp_v1_32&amp;sig=AOD64_2HFAni0PvCBl8vY8NppK7ZXfCBwQ&amp;q&amp;nis=6&amp;ch=1&amp;adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-%26gclid%3DEAIaIQobChMI1J6c76nblQMVq0b_AR0g2ghyEAAYASAAEgJPsvD_BwE&amp;ved=2ahUKEwjfipbvqduVAxVAjIkEHeMtKW4Q0Qx6BAgXEAE|
|2026-07-17 23:53:35|French bee - French Airline / A New Way of Flying / French bee|https://us.frenchbee.com/en?utm_medium=cpc&amp;utm_source=google&amp;utm_campaign=Brand__Floride__Sch--mcpc_US&amp;gad_source=1&amp;gad_campaignid=19970130464&amp;gbraid=0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-&amp;gclid=EAIaIQobChMI1J6c76nblQMVq0b_AR0g2ghyEAAYASAAEgJPsvD_BwE|
|2026-07-18 00:31:09|Erreur|https://mytrip.frenchbee.com/booking/recovery|
|2026-07-18 00:31:12|Booking|https://mytrip.frenchbee.com/bf/statics/booking/app/13.0.59-bf-booking.2/#|
|2026-07-18 00:31:24|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&amp;pf=1&amp;ai=DChsSEwjLu42pstuVAxV0MggFHY66Kv8YACICCAEQARoCbWQ&amp;co=1&amp;ase=2&amp;gclid=EAIaIQobChMIy7uNqbLblQMVdDIIBR2Ouir_EAAYASAAEgJL6PD_BwE&amp;cid=CAASugHkaIUH8rGSpXVOsJ5oAH726ukkO2jASIK4D_uJN8dXkLMdIEv0m4amBiD5QgQFex5-jaLteXrFqf_Dq9ik9mPHK8lR9dBVa_1ffzq1LEXy6cqAk_SUT9dnn9RrHrxkadNYvmXaJi37hMKTs3VzBZ41XlA_pCjP-3stuPubj1jpfVBBH8WPPJuknpX6OKaLmbmNpVmA70vJSzoepZeGVaeWdLR531LWaYlSdBp3vezoNC59UQH7jqVr5H8&amp;cce=2&amp;category=acrcp_v1_32&amp;sig=AOD64_2Bk13vtPr7GtfStgTM1KvcUosK6A&amp;q&amp;nis=6&amp;ch=1&amp;adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-%26gclid%3DEAIaIQobChMIy7uNqbLblQMVdDIIBR2Ouir_EAAYASAAEgJL6PD_BwE&amp;ved=2ahUKEwi6k4epstuVAxVhk4kEHb8NGfgQ0Qx6BAgMEAE|
|2026-07-18 00:31:24|French bee - French Airline / A New Way of Flying / French bee|https://us.frenchbee.com/en?utm_medium=cpc&amp;utm_source=google&amp;utm_campaign=Brand__Floride__Sch--mcpc_US&amp;gad_source=1&amp;gad_campaignid=19970130464&amp;gbraid=0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-&amp;gclid=EAIaIQobChMIy7uNqbLblQMVdDIIBR2Ouir_EAAYASAAEgJL6PD_BwE|
|2026-07-18 00:47:41|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&amp;pf=1&amp;ai=DChsSEwj5vd76tduVAxVJkFoFHQiBOFQYACICCAEQABoCdnU&amp;co=1&amp;ase=2&amp;gclid=EAIaIQobChMI-b3e-rXblQMVSZBaBR0IgThUEAAYASAAEgIsJfD_BwE&amp;cid=CAASugHkaLFFkdxlA6h01a1V-m1Dk5GVTzxU_Y5s0LfmZfx38gzGBYX9Q-lCXGP9TX7UNRJOrkkE21Fo-1VZA7fQcrP6mXmOusHmEljFfZJCjJZoTs_T3hrQutVxB648L2p7SgOztWYHf6b0pCoSgdffuOFGWvVCGiw2_G4xGnyOriEzuF63iAuIYjW2tX3gTzAZmj5UCZzXpY9QabmhuGavGeLndpG2WqNM4CkHLI_l_j6DfqxYJB7fK9inclk&amp;cce=2&amp;category=acrcp_v1_32&amp;sig=AOD64_3KtflkPyg2_8hKk8mdFgBjNVpP5Q&amp;q&amp;nis=6&amp;ch=1&amp;adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-%26gclid%3DEAIaIQobChMI-b3e-rXblQMVSZBaBR0IgThUEAAYASAAEgIsJfD_BwE&amp;ved=2ahUKEwjnqtn6tduVAxU0RjABHYUiBaUQ0Qx6BAgWEAE|
|2026-07-18 00:47:41|French bee - French Airline / A New Way of Flying / French bee|https://us.frenchbee.com/en?utm_medium=cpc&amp;utm_source=google&amp;utm_campaign=Brand__Floride__Sch--mcpc_US&amp;gad_source=1&amp;gad_campaignid=19970130464&amp;gbraid=0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-&amp;gclid=EAIaIQobChMI-b3e-rXblQMVSZBaBR0IgThUEAAYASAAEgIsJfD_BwE|
|2026-07-18 00:48:37|Manage my booking / French bee|https://us.frenchbee.com/en/manage-my-booking|
|2026-07-18 00:51:15|Politique de protection des données / French bee|https://www.frenchbee.com/fr/politique-protection-donnees?_ics=1784350274545&amp;irclickid=~cjqh-ea505~120WYWXNVLCDFArpqjciorljfg891SLECsid820XN&amp;_gl=1*w7l91z*_gcl_aw*R0NMLjE3ODQzNTAxMTkuRUFJYUlRb2JDaE1JLWIzZS1yWGJsUU1WU1pCYUJSMElnVGhVRUFBWUFTQUFFZ0lzSmZEX0J3RQ..*_gcl_au*OTgxMTEwMDI5LjE3ODQzNDYwMjM.*_ga*MTUyNzg5NDM1MC4xNzg0MzQ2MDIz*_ga_L4KY30N8F3*czE3ODQzNDYwMjIkbzEkZzEkdDE3ODQzNTAyMzQkajI4JGwwJGg2OTQ1NDk2MTU.*_fplc*UFZtUXdEUlBxVE03T2czb2tVOG12QlM5VDNEJTJCJTJCdk9JaHhNbzR5bWlOSnpTQUlpMWFjakF3S1NkSmRyTVBHQmM2dXN3MW02RjljRklaQ0RHQ2pieFBDWlhleE1XJTJCUjY0UTlmejA3QTVxTkxYaVNPdEZpd2hFZkdUY2JhT3RBJTNEJTNE|
|2026-07-18 11:07:52|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&amp;pf=1&amp;ai=DChsSEwiqz73KwNyVAxV1nFoFHSloOxoYACICCAEQAhoCdnU&amp;co=1&amp;ase=2&amp;gclid=CjwKCAjwyOzSBhBTEiwAmxvJ-qfiFSrKAyS2jX3Vrnox72Oow1seP4_EoWdjHz6-asX22vM5_AlxghoCoPoQAvD_BwE&amp;ei=xJZbao-SN7mUwbkPosWO-Qk&amp;cid=CAASugHkaJPYoKWAy6oBZH8AiUPNnITEHG6q-dCDjDON76SgHIWW6olfamQqssKXIVQ9Stlv8q3GiP7kCb_9dO_ZNOFoDYvzrU6fSBWZld5jpAgjNKf3dzWRoyiEztipMvP8Xn6Elb0TbXxCwF5rmTRw48tY6pttZj-Sq5Zkt8mJzkUGHcWHrYEbgHbwYLkS3D85bDbG7M0xR7r9KtTYXGARRggHlvTr2wnkoHmP1gAHEAYuFaMAWnNHLAsKTCk&amp;cce=2&amp;category=acrcp_v1_32&amp;sig=AOD64_2Z7K8y1bSG8yw6qBTLiHK1GpFS3g&amp;q&amp;sqi=2&amp;nis=6&amp;ch=1&amp;adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-%26gclid%3DCjwKCAjwyOzSBhBTEiwAmxvJ-qfiFSrKAyS2jX3Vrnox72Oow1seP4_EoWdjHz6-asX22vM5_AlxghoCoPoQAvD_BwE&amp;ved=2ahUKEwiPhLjKwNyVAxU5SjABHaKiI58Q0Qx6BAgSEAE|
|2026-07-18 11:10:16|French bee - French Airline / A New Way of Flying / French bee|https://us.frenchbee.com/en?utm_medium=cpc&amp;&amp;utm_source=google&amp;utm_campaign=Brand__Floride__Sch--mcpc_US&amp;gad_source=1&amp;gad_campaignid=19970130464&amp;gbraid=0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-&amp;gclid=CjwKCAjwyOzSBhBTEiwAmxvJ-qfiFSrKAyS2jX3Vrnox72Oow1seP4_EoWdjHz6-asX22vM5_AlxghoCoPoQAvD_BwE|
|2026-07-18 11:45:23|French bee - French Airline / A New Way of Flying / French bee|https://us.frenchbee.com/|
|2026-07-18 11:50:19|Sélection du vol|https://mytrip.frenchbee.com/booking?lang=en&amp;search=%7B%22commercialFareFamilies%22%3A%5B%22ECONEW%22%5D%2C%22travelers%22%3A%5B%7B%22passengerTypeCode%22%3A%22ADT%22%7D%2C%7B%22passengerTypeCode%22%3A%22ADT%22%7D%5D%2C%22itineraries%22%3A%5B%7B%22originLocationCode%22%3A%22MIA%22%2C%22destinationLocationCode%22%3A%22ORY%22%2C%22departureDateTime%22%3A%222026-09-10%22%7D%2C%7B%22originLocationCode%22%3A%22ORY%22%2C%22destinationLocationCode%22%3A%22MIA%22%2C%22departureDateTime%22%3A%222026-09-19%22%7D%5D%7D&amp;portalFacts=%5B%7B%22key%22%3A%22countryCode%22%2C%22value%22%3A%22US%22%7D%5D|
|2026-07-18 11:50:20|Sélection du vol|https://mytrip.frenchbee.com/booking/availability/0|
|...| | |
|2026-07-18 11:56:11|Plan de cabine|https://mytrip.frenchbee.com/booking/seatmap/ST1|
|2026-07-18 11:57:07|Plan de cabine|https://mytrip.frenchbee.com/booking/seatmap/ST2|
|2026-07-18 11:58:15|Sélection de repas|https://mytrip.frenchbee.com/booking/services/MEAL|
|2026-07-18 12:04:43|refx-title.breakfast|https://mytrip.frenchbee.com/booking/services/BREAKFAST|
|2026-07-18 12:05:41|refx-title.packs|https://mytrip.frenchbee.com/booking/services/PACKS|
|2026-07-18 12:06:27|Panier|https://mytrip.frenchbee.com/booking/shopping-cart|
|2026-07-18 12:06:40|Paiement|https://mytrip.frenchbee.com/booking/payment|
|2026-07-18 12:08:56|Confirmation|https://mytrip.frenchbee.com/booking/confirmation|
|2026-07-25 14:51:15|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&amp;pf=1&amp;ai=DChsSEwjxrIjIv-6VAxWeXH8AHSwkMUYYACICCAEQAhoCb2E&amp;co=1&amp;ase=2&amp;gclid=CjwKCAjwvZHTBhAlEiwA1ug5P9iJ19XB9KzTdIUdNV1r4kflJ2e65EPRn-5xizENfeFuO2ZDCdTZnxoCKJkQAvD_BwE&amp;cid=CAASugHkaKgoTltm8WZFozkUn80ODM8k8j_d-lyo9A8Ce1zQkFtIB0bcE3TFoSR17Xm_1rWIG6ujRNoISjh6KlBdhkTWiAt40KdI6VgB9LJw1aXMzfSbSHPSsL5Adi4OPpWvCpU5VIrrt4rtYXUsiN-NWhgMiYyeiV7r17-VtbQdSmQHfBzYtFr8-oY78jxyYHrkOn26Kcy8yh24L92Dmq4YJnBzzRPjESs091_jtE9WH7K-dtWIIt5wG5plbbA&amp;cce=2&amp;category=acrcp_v1_32&amp;sig=AOD64_1LEjG4NZXf_BNqj5JFEu5aeMsvyg&amp;q&amp;nis=6&amp;ch=1&amp;adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTd3kDGbn8vV3-_uHqPfqWtku%26gclid%3DCjwKCAjwvZHTBhAlEiwA1ug5P9iJ19XB9KzTdIUdNV1r4kflJ2e65EPRn-5xizENfeFuO2ZDCdTZnxoCKJkQAvD_BwE&amp;ved=2ahUKEwjP6ILIv-6VAxW9mWoFHSM1EGkQ0Qx6BAgNEAE|
|2026-07-25 14:51:15|French bee - French Airline / A New Way of Flying / French bee|https://us.frenchbee.com/en?utm_medium=cpc&amp;utm_source=google&amp;utm_campaign=Brand__Floride__Sch--mcpc_US&amp;gad_source=1&amp;gad_campaignid=19970130464&amp;gbraid=0AAAAADBHuTd3kDGbn8vV3-_uHqPfqWtku&amp;gclid=CjwKCAjwvZHTBhAlEiwA1ug5P9iJ19XB9KzTdIUdNV1r4kflJ2e65EPRn-5xizENfeFuO2ZDCdTZnxoCKJkQAvD_BwE|
|2026-07-25 14:52:01|Help Center French bee|https://support.frenchbee.com/hc/en-us?_ics=1785005519761&amp;irclickid=~cjqh-ea505~120WYWXNVLCDFArpqjciorljfg8930WNLFApfa~7Y&amp;_gl=1*1sggm5r*_gcl_aw*R0NMLjE3ODUwMDU0NzkuQ2p3S0NBand2WkhUQmhBbEVpd0ExdWc1UDlpSjE5WEI5S3pUZElVZE5WMXI0a2ZsSjJlNjVFUFJuLTV4aXpFTmZlRnVPMlpEQ2RUWm54b0NLSmtRQXZEX0J3RQ..*_gcl_au*OTgxMTEwMDI5LjE3ODQzNDYwMjMuNDE2MDE1OTIuMTc4NDM5OTgxNC4xNzg0Mzk5ODEzLjE2NjIwMzI2NzguMTc4NDM5MDkzOC4xNzg0Mzk5ODEz*_ga*MTUyNzg5NDM1MC4xNzg0MzQ2MDIz*_ga_L4KY30N8F3*czE3ODUwMDU0NzckbzUkZzEkdDE3ODUwMDU0ODkkajQ4JGwwJGg1OTQ0NDEyMTg.*_fplc*dk9ORmY0JTJGNWdscG9ZOW5xaFhqWUt3ZlJybGY4bGo1blNqWFd6cjRJcmE2MDhhdm9hSFUwT0VwbW5EbUdWZU8lMkZ3ZUp0SDd3RjRENTFsNVNxSktybkM4MEt5Q3FXUnNobWhuQ1VqM0l4TXNNQjVnenNOcU45WEhud3plTnE4QSUzRCUzRA..|
|2026-07-25 14:52:09|Our destinations / French bee|https://us.frenchbee.com/en/destinations|
|2026-07-25 14:52:23|Submit a request – Help Center French bee|https://support.frenchbee.com/hc/en-us/requests/new|
|2026-07-25 14:54:48|Help Center French bee|https://support.frenchbee.com/hc/en-us?return_to=%2Fhc%2Frequests|
|2026-07-25 14:55:06||https://support.frenchbee.com/verification/anonymous_request/WXkotm64h34WmxX5qoffKIolS?locale=1|
|2026-07-25 14:55:08|FAQ French bee|https://support.frenchbee.com/hc/en-us/requests/verification/success|
|2026-07-25 14:55:13|Help Center French bee|https://support.frenchbee.com/hc/en-us|


```sql

SELECT  datetime(v.visit_time / 1000000 - 11644473600, 'unixepoch', 'localtime') AS visit_time, u.title as title, u.url as url FROM 
visits v JOIN urls u ON u.id = v.url 
WHERE u.url LIKE '%frenchbee%' ORDER BY v.visit_time ASC;

```

| `visit_time`|`title` | `url`|
|----------------------------------------|-----------------------------------------------|---------------------------|
|2026-07-17 23:40:21|Redirection en cours / French bee|https://app.your-merchandise.com/forward?site-id=3f12308a-e2ed-4da7-ac6f-ddf77faaa6f4&compilation-id=02f88f4d-abdd-405b-a2c2-8a952dbd85c4&link-id=link_3&redirect=https%253A%252F%252Fwww.frenchbee.com%252Ffr%252Fredirection_amadeus%253Fdd%253D12092026%2526tt%253DR%2526rd%253D19092026%2526de%253DMIA%2526re%253DORY%2526ad%253D2%26so%3DscEMR1|
|2026-07-17 23:40:34|Booking|https://mytrip.frenchbee.com/booking?lang=fr&search=%7B%22commercialFareFamilies%22%3A%5B%22ECONEW%22%5D%2C%22travelers%22%3A%5B%7B%22passengerTypeCode%22%3A%22ADT%22%7D%2C%7B%22passengerTypeCode%22%3A%22ADT%22%7D%5D%2C%22itineraries%22%3A%5B%7B%22originLocationCode%22%3A%22MIA%22%2C%22destinationLocationCode%22%3A%22%22%2C%22departureDateTime%22%3A%222026-09-12%22%7D%2C%7B%22originLocationCode%22%3A%22%22%2C%22destinationLocationCode%22%3A%22MIA%22%2C%22departureDateTime%22%3A%222026-09-19%22%7D%5D%7D&portalFacts=%5B%7B%22key%22%3A%22countryCode%22%2C%22value%22%3A%22FR%22%7D%5D&_gl=1*162hbjg*_gcl_au*OTgxMTEwMDI5LjE3ODQzNDYwMjM.*_ga*MTUyNzg5NDM1MC4xNzg0MzQ2MDIz*_ga_L4KY30N8F3*czE3ODQzNDYwMjIkbzEkZzEkdDE3ODQzNDYwMjQkajU4JGwwJGg2OTQ1NDk2MTU.*_fplc*UFZtUXdEUlBxVE03T2czb2tVOG12QlM5VDNEJTJCJTJCdk9JaHhNbzR5bWlOSnpTQUlpMWFjakF3S1NkSmRyTVBHQmM2dXN3MW02RjljRklaQ0RHQ2pieFBDWlhleE1XJTJCUjY0UTlmejA3QTVxTkxYaVNPdEZpd2hFZkdUY2JhT3RBJTNEJTNE|
|2026-07-17 23:40:36|Sélection du vol|https://mytrip.frenchbee.com/booking/availability/0|
|2026-07-17 23:41:07|Booking|https://mytrip.frenchbee.com/bf/statics/booking/app/13.0.59-bf-booking.2/#|
|2026-07-17 23:53:35|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&pf=1&ai=DChsSEwjUnpzvqduVAxWrRv8BHSDaCHIYACICCAEQARoCbWQ&co=1&ase=2&gclid=EAIaIQobChMI1J6c76nblQMVq0b_AR0g2ghyEAAYASAAEgJPsvD_BwE&cid=CAASugHkaP7LsesU8fHVhOl7niz8Lp-tLQjRt2kb7TIMWciRw8M2ao9b_paYLpVPsXKGe7PxiUjjdKoyn_P0-VL_mF3Kpy15jFesvKRcxt_CtjqbHKj2O5ytSOygSyT_whWhuSyQ87dNIt9UgGtQfqka9UPWnz426gPyeyHqO1a_x2msE0F_b_gW7XFIqNeC6hJ0CFGCHVg5If1exXkJ2Ih9oeO7_3-SSqwNcUSdPxkJ2yQbUHsucMj-MOt87o4&cce=2&category=acrcp_v1_32&sig=AOD64_2HFAni0PvCBl8vY8NppK7ZXfCBwQ&q&nis=6&ch=1&adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-%26gclid%3DEAIaIQobChMI1J6c76nblQMVq0b_AR0g2ghyEAAYASAAEgJPsvD_BwE&ved=2ahUKEwjfipbvqduVAxVAjIkEHeMtKW4Q0Qx6BAgXEAE|
|2026-07-17 23:56:41|Sélection du vol|https://mytrip.frenchbee.com/booking/availability/1|
|2026-07-17 23:57:08|Panier|https://mytrip.frenchbee.com/booking/shopping-cart|
|2026-07-17 23:57:45|Informations voyageurs|https://mytrip.frenchbee.com/booking/traveler/0|
|2026-07-18 00:03:04|Informations voyageurs|https://mytrip.frenchbee.com/booking/traveler/1|
|2026-07-18 00:04:38|Plan de cabine|https://mytrip.frenchbee.com/booking/seatmap/ST1|
|2026-07-18 00:12:29|Panier|https://mytrip.frenchbee.com/booking/shopping-cart|
|2026-07-18 00:13:17|Sélection de repas|https://mytrip.frenchbee.com/booking/services/MEAL|
|2026-07-18 00:19:31|Panier|https://mytrip.frenchbee.com/booking/shopping-cart|
|2026-07-18 00:20:55|Plan de cabine|https://mytrip.frenchbee.com/booking/seatmap/ST1|
|2026-07-18 00:23:13|Panier|https://mytrip.frenchbee.com/booking/shopping-cart|
|2026-07-18 00:23:28|Plan de cabine|https://mytrip.frenchbee.com/booking/seatmap/ST2|
|2026-07-18 00:26:26|Panier|https://mytrip.frenchbee.com/booking/shopping-cart|
|2026-07-18 00:27:28|Informations voyageurs|https://mytrip.frenchbee.com/booking/traveler/1|
|2026-07-18 00:30:27|Erreur|https://mytrip.frenchbee.com/booking/recovery|
|2026-07-18 00:30:48|Booking|https://mytrip.frenchbee.com/bf/statics/booking/app/13.0.59-bf-booking.2/#|
|2026-07-18 00:31:09|Informations voyageurs|https://mytrip.frenchbee.com/booking/traveler/1|
|2026-07-18 00:31:09|Erreur|https://mytrip.frenchbee.com/booking/recovery|
|2026-07-18 00:31:12|Booking|https://mytrip.frenchbee.com/bf/statics/booking/app/13.0.59-bf-booking.2/#|
|2026-07-18 00:31:24|French bee - French Airline / A New Way of Flying / French bee|https://www.google.com/aclk?sa=L&pf=1&ai=DChsSEwjLu42pstuVAxV0MggFHY66Kv8YACICCAEQARoCbWQ&co=1&ase=2&gclid=EAIaIQobChMIy7uNqbLblQMVdDIIBR2Ouir_EAAYASAAEgJL6PD_BwE&cid=CAASugHkaIUH8rGSpXVOsJ5oAH726ukkO2jASIK4D_uJN8dXkLMdIEv0m4amBiD5QgQFex5-jaLteXrFqf_Dq9ik9mPHK8lR9dBVa_1ffzq1LEXy6cqAk_SUT9dnn9RrHrxkadNYvmXaJi37hMKTs3VzBZ41XlA_pCjP-3stuPubj1jpfVBBH8WPPJuknpX6OKaLmbmNpVmA70vJSzoepZeGVaeWdLR531LWaYlSdBp3vezoNC59UQH7jqVr5H8&cce=2&category=acrcp_v1_32&sig=AOD64_2Bk13vtPr7GtfStgTM1KvcUosK6A&q&nis=6&ch=1&adurl=https://us.frenchbee.com/en?utm_medium%3Dcpc%26utm_source%3Dgoogle%26utm_campaign%3DBrand__Floride__Sch--mcpc_US%26gad_source%3D1%26gad_campaignid%3D19970130464%26gbraid%3D0AAAAADBHuTdACrkcVlPnUCd-oSQ7yvOQ-%26gclid%3DEAIaIQobChMIy7uNqbLblQMVdDIIBR2Ouir_EAAYASAAEgJL6PD_BwE&ved=2ahUKEwi6k4epstuVAxVhk4kEHb8NGfgQ0Qx6BAgMEAE|
|2026-07-18 00:32:11|Sélection du vol|https://mytrip.frenchbee.com/booking/availability/0|


now switch to Cache Data file inspection - limit by `LastWriteTime` 

```powershell

$cache = "$env:LOCALAPPDATA\Google\Chrome\User Data.BACKUP\Default\Cache\Cache_Data"
cd $cache
Get-ChildItem -path '.' -File |
where-object { $_.LastWriteTime -ge [datetime]"2026-09-07 09:15:00" -and $_.LastWriteTime -le [datetime]"2026-09-07 10:00:00" } | 
sort-object LastWriteTime | select-Object LastWriteTime, Length, Name
```

```text
  LastWriteTime       Length Name
  -------------       ------ ----
  9/7/2026 9:23:14 AM  80471 f_00216c
  9/7/2026 9:23:14 AM 128432 f_00216d
  9/7/2026 9:25:40 AM  24534 f_00218b
  9/7/2026 9:29:43 AM  48256 f_002237
  9/7/2026 9:30:11 AM  56176 f_002245
  9/7/2026 9:30:11 AM  29044 f_002247
```

```powershell
$cache = "$env:LOCALAPPDATA\Google\Chrome\User Data.BACKUP\Default\Cache\Cache_Data"
cd $cache

$files = @( 'f_00216c', 'f_00216d', 'f_00218b', 'f_002237', 'f_002245', 'f_002247' )

foreach ($name in $files) {
  $file = Join-Path $cache $name
  $bytes = [System.IO.File]::ReadAllBytes($file)

  Write-Host "`n===== $name ($($bytes.Length) bytes) ====="

  ($bytes[0..31] | ForEach-Object { "{0:X2}" -f $_ }) -join ' '

  [regex]::Matches( [System.Text.Encoding]::ASCII.GetString($bytes), '[ -~]{30,}') | select-object -First 2 | foreach-object { write-output $_.Value }
}
```
> NOTE : for Clipboard copy paste need to concatenate first
> ```powershell
> foreach ($name in $files) {  $file = Join-Path $cache $name ;  $bytes = [System.IO.File]::ReadAllBytes($file); Write-Host "`n===== $name ($($bytes.Length) bytes) ====="; ($bytes[0..31] | ForEach-Object { "{0:X2}" -f $_ }) -join ' ';  [regex]::Matches( [System.Text.Encoding]::ASCII.GetString($bytes), '[ -~]{30,}') | select-object -First 2 | foreach-object { write-output $_.Value };}
> ```
```text
  
===== f_00216c (80471 bytes) =====
1F 8B 08 00 00 00 00 00 00 FF AC 5A 7B 57 DB BA B2 FF 9F 4F 11 72 7A B2 6D 22 42 42 81 B6 31 4E
w-w1Q9?????M?i UN???????U0R^N????F???w@70??
?[,h??zr4;????m??6;i??6?h?/m???D??:??a?

===== f_00216d (128432 bytes) =====
1F 8B 08 00 00 00 00 00 00 FF EC BD 67 57 1C C9 B6 28 F8 5D BF 02 F2 EA E8 54 36 49 A9 40 A6 BB
W?}???HSH}?{o??5???4?aw??~[?Q?4
???WhQ??V?m????????R??????T~??`)XHE?

===== f_00218b (24534 bytes) =====
F1 A0 BF 9F 70 37 C3 C6 01 40 D8 8E 91 66 22 82 8D 03 80 F1 FD 9B 06 A8 1E 8F 1E C3 4D 1F F2 A8
?n??]E?B9.?????1s????W???4?wt???H?*?)??;???boiF??(?z???>
t????????V?3?_?w??t??????q?????;>?k??&????????????c

===== f_002237 (48256 bytes) =====
77 4F 46 32 00 01 00 00 00 00 BC 80 00 15 00 00 00 01 E0 20 00 00 BC 05 00 01 00 00 00 00 00 00
)?P?=???U?}???z`?/d?????????J??S??????H????
?????~???J.?]s?,?lKh"d?\2?I??jA"J3d??W"

===== f_002245 (56176 bytes) =====
81 E2 81 98 28 F2 38 00 1A 5D 99 28 82 8D 23 60 60 64 BB 73 26 22 D8 38 00 60 D4 CF 9E D0 73 71
?c?,???V?W?????|?:U???#~S-7S?U??????.K?2Ll%?
#D?lis????h?)?!2?????[U?????D??o%?<V?#`=?5?

===== f_002247 (29044 bytes) =====
0A 44 2F A2 08 36 0E 00 84 F8 6F 1D 46 22 84 8D 03 80 10 EB 05 03 7A 1C EE 58 4D 83 7D 32 76 C8
????1???ZC?+??'\??Y?M?j????"????
r*\w?@????4?f+y????????fN8@r?C???#???l?
 
```


  What the signatures tell us

  |File|Size|Signature|Interpretation|
  |----|----|---------|----------------|
  |f_00216c|80 KB|1F 8B 08|gzip-compressed content|
  |f_00216d|128 KB|1F 8B 08|gzip-compressed content|
  |f_00218b|24 KB|unknown binary|possibly encoded/compressed/cache object|
  |f_002237|48 KB|77 4F 46 32|WOFF2 web font|
  |f_002245|56 KB|unknown binary|likely binary resource|
  |f_002247|29 KB|unknown binary|likely binary resource|


  ```
  $cache = "$env:LOCALAPPDATA\Google\Chrome\User Data.BACKUP\Default\Cache\Cache_Data"
  cd $cache

  foreach ($name in @("f_00216c", "f_00216d")) {
      $file = Join-Path $cache $name

      $input = [System.IO.File]::OpenRead($file)
      $gzip = New-Object System.IO.Compression.GZipStream(
          $input,
          [System.IO.Compression.CompressionMode]::Decompress
      )

      $output = New-Object System.IO.MemoryStream
      $gzip.CopyTo($output)

      $gzip.Dispose()
      $input.Dispose()

      $bytes = $output.ToArray()
      $output.Dispose()

      $text = [System.Text.Encoding]::UTF8.GetString($bytes)

      Write-Host "`n===== $name : decompressed $($bytes.Length) bytes ====="

      # First 1000 characters
      Write-Host $text.Substring(0, [Math]::Min(1000, $text.Length))

      Write-Host "`n--- interesting strings ---"

      [regex]::Matches(
          $text,
          '.{0,100}(?:baggage|bagage|personal.?item|small.?item|dimension|40.{0,30}30|30.{0,30}20|15.?cm|20.?cm).{0,200}',
          [System.Text.RegularExpressions.RegexOptions]::IgnoreCase
      ) |
      Select-Object -First 20 |
      ForEach-Object {
          Write-Host $_.Value
      }
  }
  ```


  ```text
   Copyright The Closure Library Authors.
   SPDX-License-Identifier: Apache-2.0
  */
  /*

   Copyright Google LLC
   SPDX-License-Identifier: Apache-2.0
  */
  /*
   SPDX-License-Identifier: Apache-2.0
  */
  /*

   Copyright 2024 Google, Inc
   SPDX-License-Identifier: MIT
  */
  /*

   Copyright 2005, 2007 Bob Ippolito. All Rights Reserved.
   Copyright The Closure Library Authors.
   SPDX-License-Identifier: MIT
  */
  /*

   Copyright Google LLC All Rights Reserved.

   Use of this source code is governed by an MIT-style license that can be
   found in the LICENSE file at https://angular.dev/license
  */
  var ja,aaa,Ia,Ja,baa,Ma,eb,ub,wb,xb,yb,Bb,Fb,faa,Vb,$b,ac,bc,cc,ec,fc,mc,gaa,haa,oc,qc,wc,xc,Ac,kaa,jaa,yc,maa,Jc,Sc,Tc,ed,jd,md,rd,nd,Kd,Gd,Hd,R
  d,uaa,ge,xaa,zaa,ne,ye,Aaa,Fe,Ge,Ie,Ke,Ne,We,Ve,Te,Xe,Ye,qf,Daa,Ff,Gf,Kf,Eaa,Pf,rg,Haa,Iaa,Jaa,Kaa,xg,Ag,Mg,Sg,Ug,Vg,$g,bh,fh,jh,hh,Raa,ph,Saa,Ta
  a,Uaa,Vaa,Waa,Xaa,Lh,Y

  --- interesting strings ---
  .Xk[2]&8),tca=!!(_.Xk[2]&2),uca=!!(_.Xk[2]&4);var gl,Yb,wca,ke,tb,il,jl;gl=Wk(1,!0);Yb=el?jca:Wk(610401301,!1);_.vca=el?fl||!kca:Wk(1331761403,!0
  );wca=el?lca:Wk(651175828,!1);ke=el?fl||!mca:Wk(748402147,!0);_.xca=el?nca:Wk(861377723,!1);_.hl=el?oca:Wk(861377724,!1);_.yca=el?fl||!pca:Wk(869
  336904,gl);_.zc

  ===== f_00216d : decompressed 362284 bytes =====
  "use strict";_F_installCss(":root{--boq-chrometransition-background:#eee;--boq-chrometransition-active-background-opacity:0.8}.KL4X6e{background:
  var(--boq-chrometransition-background);bottom:0;left:0;opacity:0;position:absolute;right:0;top:0}.TuA45b{opacity:var(--boq-chrometransition-activ
  e-background-opacity)}sentinel{}");
  this.default_OneGoogleWidgetUi=this.default_OneGoogleWidgetUi||{};(function(_){var window=this;
  try{
  _.EA=function(a){if(a instanceof _.DA)return a.j;throw Error("x");};_.FA=function(a){return new _.DA(_.Pa,a[0].toLowerCase())};_.GA=function(a,b)
  {var c,d,e;return _.Mk(function(f){if(f.l==1){_.hg(a);a[_.Sf]||_.Qf(a);if(a.zd&&!a[_.Sf])return f.return(Promise.resolve(_.jg(a,b)));c=new Promis
  e(function(g,h){a[_.Sf](g,function(k){_.nf(k)?g(k):h(k)})});return f.j(c,2)}d=f.o;return f.return(_.ig(d,_.Of(a),(e=b)==null?void 0:e.optional))}
  )};
  _.HA=function(a,b){_.hg(a);var c=_.fg();if(a[_.Sf]){var d=null;return function(){return d?d:d=_.gg(c,function(){return _.GA(a,b)})}}va

  --- interesting strings ---
  ribute("dir")}};hha=/<[^>]*>|&[^;]+;/g;lha=RegExp("[A-Za-z\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02b8\u0300-\u0590\u0900-\u1fff\u200e\u2c00-\ud801\ud
  804-\ud839\ud83c-\udbff\uf900-\ufb1c\ufe00-\ufe6f\ufefd-\uffff]");
  jha=RegExp("^[^A-Za-z\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02b8\u0300-\u0590\u0900-\u1fff\u200e\u2c00-\ud801\ud804-\ud839\ud83c-\udbff\uf900-\ufb1c\
  ufe00-\ufe6f\ufefd-\uffff]*[\u0591-\u06ef\u06fa-\u08ff\u200f\ud802-\ud803\ud83a-\ud83b\ufb1d-\ufdff\ufe70-\ufefc]");kha=/^http:\/\/.*/;_.nha=RegE

  xp("[A-Za-z\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02b8\u0300-\u0590\u0900-\u1fff\u200e\u2c00-\ud801\ud804-\ud839\ud83c-\udbff\uf900-\ufb1c\ufe00-\ufe
  6f\ufefd-\uffff][^\u0591-\u06ef\u06fa-\u08ff\u200f\ud802-\ud803\ud83a-\ud83b\ufb1d-\ufdff\ufe70-\ufefc]*$");
  802-\ud803\ud83a-\ud83b\ufb1d-\ufdff\ufe70-\ufefc][^A-Za-z\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02b8\u0300-\u0590\u0900-\u1fff\u200e\u2c00-\ud801\ud
  804-\ud839\ud83c-\udbff\uf900-\ufb1c\ufe00-\ufe6f\ufefd-\uffff]*$");iha=/\s+/;mha=/[\d\u06f0-\u06f9]/;

  ```
  ```powershell

  $cache = "$env:LOCALAPPDATA\Google\Chrome\User Data.BACKUP\Default\Cache\Cache_Data"
  cd $cache

  $files = @(
      "f_00218b",
      "f_002245",
      "f_002247"
  )

  foreach ($name in $files) {
      $file = Join-Path $cache $name
      $bytes = [System.IO.File]::ReadAllBytes($file)

      Write-Host "`n========================================"
      Write-Host "$name : $($bytes.Length) bytes"
      Write-Host "========================================"

      # ASCII strings
      $ascii = [System.Text.Encoding]::ASCII.GetString($bytes)

      Write-Host "`n-- ASCII strings --"

      [regex]::Matches($ascii, '[ -~]{12,}') |
          Select-Object -First 40 |
          ForEach-Object {
              Write-Host $_.Value
          }

      # UTF-16LE strings
      $unicode = [System.Text.Encoding]::Unicode.GetString($bytes)

      Write-Host "`n-- UTF-16LE strings --"

      [regex]::Matches($unicode, '[\x20-\x7E]{8,}') |
          Select-Object -First 40 |
          ForEach-Object {
              Write-Host $_.Value
          }
  }
  ```