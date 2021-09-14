table_plan <- drake_plan(
  
 
   define.table = tribble(
    ~ "Code", ~ "Inclusion criteria and definitions", ~ "Examples",
    "Transparency", "Participant uses the words transparent, transparency or open to scrutiny.", "Science based on data and methods that are completely transparent...",
    "Accessibility/ Availability", "Participant uses the words available or accessible or both in combination. Includes being understandable. Can reference being accessible to other scientists and/or to the public. Includes openly.", "The sharing of scientific theory, data and results such that it can be accessed by anyone...",
    "Sharing codes/ methods", "Codes or methods are shared publicly and are available for others to use. Includes the idea that these methods should be free to access.", "I define open science as a transparent process of doing Research, where the data, Methods, analyses and results are made available for others to replicate the study or analyse the data further.",
    "Sharing data", "Data is shared publicly and are available for others to use. Data re-use is included here. Includes the word interoperable outside of FAIR. Includes the idea that data sharing platforms should be free.", "To have free, available data and papers. I think this work should be easily accessible to anyone. Data should not be altered (eg to hide information) and instead published with the intent of it being used by other researchers.",
    "Working collaboratively with peers or other stakeholders", "Open science facilitates collaboration or includes having stakeholders be a part of the research.", "To me, Open Science means operating in a transparent manner. This could include making data and code available, facilitating research that might not involve yourself (which could be accomplished by making data and code available) and being open to working collaboratively to accomplish a common goal", 
    "Replication/ Reproducibility", "Open science makes scientific findings replicable, reproducible, and/or verifiable.", "...the methods used for analysing/interpreting the data are described/published in a way that makes them reproducible...",
    "Open access publications", "Publications are open access (i.e., not behind a paywall). May specifically refer to paywalls as a barrier. Includes pre-prints.", "Scientific publications and the data used (can be anonymised) should easily without costs be accessible through internet.", 
    "FAIR principles", "Specifically refers to FAIR principles: Findable, accessible, interoperable, and reproducible.", "open sharing of data according to FAIR standards",
    "Inclusivity", "Indicates OS is inclusive and/or equitable; can refer to individuals or institutions and can reference making science possible for those who are not able to find funding.", "Open science is research and knowledge that is available to everyone, regardless of social or economic situation.",
    "Relationship between OS and Education", "Participant discusses relationship between OS and education; however, may have views ranging from OS being part of education to the two concepts being in conflict.", "In our education, we work on incorporating open science and training around data management and sharing for our students.",
    "Data policies & practices", "This includes metadata and following policies about data organization and documentation. Also includes standardization of sharing data.", "It implies using standards for structuring and formatting data, standards for metadata, suitable web protocols, open source software and applications.",
    "Responsible & available to the public", "Specifically mentions that the public should have access to the data, methods, and or publications. Also, includes being responsible with public funds and making the information understandable to the public. Includes Citizen Science", "When data is originally collected with public funding it should continue benefiting publicly funded research, and enhancing collaboration.",
    "Other", "Anything that is a one-off not included in the above or that is difficult to interpret. Includes open peer review, science done right, and proper credit. Includes that open science is fundable.", "science made well"
  ),
  
  
  define.freq.table = tribble(
    ~"Code or Category", ~"Number of occurrences in sample responses (before workshop; n=60)", ~"Number of occurrences in sample responses (after workshop; n=38)",
    "Sharing data", "50", "29",
    "Accessibility/Availability", "38", "14",
    "Sharing codes/methods", "36", "23",
    "Transparency", "28", "19",
    "Open access publications", "24", "12",
    "Replication/Reproducibility", "19", "13",
    "FAIR principles", "11", "6",
    "Responsible & available to the public", "9", "10",
    "Data policies & practices", "7", "9",
    "Inclusivity", "5", "2",
    "Working collaboratively with peers or other stakeholders", "5", "6",
    "Relationship between OS and Education", "3", "4"
    ),
    
  
  hinder.table = tribble(
    ~"Code", ~"Inclusion criteria and definitions", ~"Examples",
    "Cost", "Lack of funding for publications or other funds needed to engage in OS.", "Lack of funding to pay for fees in open access journals.",
    "Insufficient incentives", "There is no incentive to engage in OS from universities or funding agencies.", "In the past years, there was little motivation to engage in Open Science.",
    "Collaborators not using OS", "When collaborators are wary or do not use OS, it makes it harder for them to use OS. This means they don't have opportunities to use OS.","Fellow scientists have been a bit skeptical about the idea of ''giving away'' their data.",
    "Fear of critique", "Afraid of people being overly critical of their work because it's available.", "Fear of data being used to show that my work is wrong and flawed.",
    "Legal concerns", "Concern about either intellectual property, patent law, or data management law, such as GDPR not allowing them to share data.", "My data is not mine but rather has been funded and collected by a country's regulatory agency. Otherwise I would freely make it available.",
    "Insufficient knowledge", "Don't know how to use OS. Afraid of trying OS platforms because they don't know how to use them.", "Initially it was a lack of knowledge on how and where they could share code and data.",
    "Time", "Lack the time or it takes more time.", "Lack of time. Work tasks and family life takes almost all available time.",
    "More work", "Engaging in OS takes more work. This is used when it's unclear that they are concerned about it taking more time, but specifically mention it taking more work or effort.", "It requires extra work, setting up the data access and tidying up scripts and writing documentation.",
    "Lack of guidelines", "OS platforms or journals are unclear about how to upload data, data standards, meta data standards or other guidelines that make it difficult to know how to use them. Can also include if guidelines are regularly changing.", "There are no clearly defined expectations, boundaries and mechanisms by which researchers can engage. Also it is never clear what precisely open science requires of me as a data contributor.",
    "Want to get credit", "Worried that OS practices, especially around data, will prevent them from getting proper credit for their work.", "I can't speak from the experience but I would say fear of data being stolen or misused and no acknowledgement for the particular research.",
    "Other/Vague", "Unclear what they mean.", "",
    "Nothing", "Specifically states that there is nothing hindering them.", ""
  ),
  
  
  help.table = tribble(
    ~"Code", ~"Inclusion criteria and definitions", ~"Examples",
    "Money", "Having the money to engage such as money to pay for open publications.", "Institutional funds for open access publications.",
    "Resource availability", "The existence of functional OS platforms, programs, or data that are available for use.", "Online resources that are well written for relative beginners. User friendly platforms (e.g. OSF) that allow integration of multiple people from multiple institutions, and multiple types of files.",
    "Social support", "Encouragements or other support from their peers. This includes having a local or online community and the culture or movement around open science.", "Working with people who are collaborative and like minded.",
    "Having knowledge", "They know how OS, including platforms or programs, works. Can include having taken classes/workshops. They have the necessary information to use these open resources. Implies a knowledge barrier that was overcome.", "Increased information about options for open science.",
    "Structural support", "Programs or policies that support the adoption or use of OS that are provided by their department, institution, funding agency or journal requirements. This includes statements of encouragement from individuals in leadership positions. Institutional money for paying for publication costs goes under money not here.", "Peer support and organisation's own ICT department.",
    "Intrinsic motivation", "Reasons for engaging in open science that relate to positive or negative internal motivators such as wanting to be able to do particular types of experiments, feeling a need to pay it forward, their personal career development, or a personal philosophy that it's the right thing to do.", "Knowing that I have benefited from the resources provided by others and I have an obligation to ''pay it forward''.",
    "Prior success with OS", "Have had a positive past experience with OS that makes them more interested in continuing to engage in OS.", "I have been able to access other people's data and found it very useful in my career development hence a great motivation to do likewise."
  ),
    
  
  hinder.freq.table = tribble(
    ~"Code", ~"Number of occurrences in sample responses (Part I; n=60)",
    "Lack of guidlines", "15",
    "Time", "15",
    "Insufficient knowledge", "15",
    "Collaborators not using OS", "13",
    "Other/Vague", "12", 
    "Cost", "11", 
    "Nothing", "7",
    "More work", "5",
    "Insufficient incentives", "5",
    "Legal concerns", "4",
    "Want to get credit", "3", 
    "Fear of critique", "3"
  ),
  
  help.freq.table = tribble(
    ~"Code", ~"Number of occurrences in sample responses (Part I; n=60)",
    "Social support", "20",
    "Resource availability", "20",
    "Intrinsic motivation", "13",
    "Structural support", "13",
    "Having knowledge", "8", 
    "Other/Vague", "6", 
    "Prior success with OS", "4",
    "Money", "4"
  )
  
  )
