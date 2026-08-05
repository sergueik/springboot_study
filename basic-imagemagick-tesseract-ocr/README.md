### Usage

```
docker pull minidocks/imagemagick
docker pull jitesoft/tesseract-ocr
```	
This experiment evaluates whether a simple OCR pipeline (ImageMagick + Tesseract) can extract reusable business knowledge from workflow diagrams

#### Control experiment: plain text

![plain text](images/text.png)

```sh
./scan_screenshot.sh images/text.png
```
```text
Lorem ipsum dolor sit amet, consectetuer
adipiscing elit. Maecenas porttitor congue
massa. Fusce posuere, magna sed pulvinar
ultricies, purus lectus malesuada libero, sit
amet commodo magna eros quis urna.
```

A workflow diagram is not primarily a text artifact. Even a simple "no fancy" workflow diagram typically contains far more structural information than textual information. The labels may represent only a small fraction of the artifact, while the majority of the representation describes shapes, connectors, positioning, grouping, and visual relationships

In addition, many workflow formats are shared with graphical IDEs or modeling tools. The file may contain a significant amount of authoring metadata: coordinates, layout information, editor state, object identifiers, style definitions, and other information required by the tool. This information is useful to the authoring environment but has little direct business meaning.

OCR extracts the visible labels, but it discards the very information that makes the diagram a process: the topology and relationships
### Challenges
#### Challenge 1: workflow diagram
`diagram1.mermaid`:
```code
flowchart LR

Start((Order Received))
Cond1{"Customer account active?"}
Cond2{"Order value exceeds $10,000?"}
Then1["Create shipment"]
Then2["Request manager approval"]
Else["Reject order and notify customer"]
End((Process Complete))
Start --> Cond1
Cond1 -->|Yes| Cond2
Cond1 -->|No| Else
Cond2 -->|Yes| Then2
Cond2 -->|No| Then1
Then1 --> End
Then2 --> End
Else --> End
```
![diagram1](images/diagram1.png)

Can OCR recover the business process from the rendered diagram?

```sh
./scan_screenshot.sh images/diagram1.png 
```
```text
Order value exceeds $10,000?

mplete |

/
```
Activity labels were discovered, but decision logic and branching were gone.

#### Challenge 2: business process

`diagram2.mermaid`:
```code
flowchart LR

Start([Purchase request received])
Decision{"Has the purchasing manager approved the request?"}
Approve["Create purchase order and notify supplier"]
Reject["Reject request and notify requestor with explanation"]
End([Process completed])

Start --> Decision
Decision -->|Yes| Approve
Decision -->|No| Reject
Approve --> End
Reject --> End
```
![plain text](images/diagram2.png)

```sh
./scan_screenshot.sh images/diagram2.png
```
```text
Create purchase order and notify supplier

Reject request and notify requestor with explanation

Process completed
```

Observation:
The extracted text reads like a business process description, 
but it no longer conveys the rule that determines which path is executed

### Findings

|Business information |OCR recovered |
|-----------------|-----------|
|Activity labels       | 	✅ Mostly    |
|Decision conditions |	⚠️ Partial|
|Branching logic	 |❌         |
|Sequence	         |❌         |
|Process semantics	 |❌         |

### Note

A typical workflow diagram contains far more structural information than textual information. The labels may occupy only a small fraction of the artifact, while the majority of the representation describes geometry: nodes, edges, positions, grouping, routing, and visual relationships. OCR extracts mainly the labels and discards most of the structure.

A workflow diagram is already a highly optimized visual representation. The majority of its information content is *not* text.

A large fraction of the file representation may be:
  * geometry (coordinates, bounding boxes, connectors),
  * rendering instructions,
  * editor metadata,
  * collaboration/versioning information,
  * application-specific state

### Troubleshooting

```text
Error, could not create TXT output file: Permission denied
cat: /tmp/tmp.Emwe1AX3aD/result.txt: No such file or dir
```
check the image configuration
```sh
docker inspect jitesoft/tesseract-ocr | grep -i User
```

```text
"User": "tesseract",
```

### Cleanup
```
docker image rm jitesoft/tesseract-ocr:latest minidocks/imagemagick:latest
```


### Unrelated

The next step is understanding what information exists, categorizing it, and identifying the relationships that make it valuable.

---

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
