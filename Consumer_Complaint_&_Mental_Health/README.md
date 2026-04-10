[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/GG9pnQCZ)
# ✏️ Spring Term Summative

*Author: Dr Ghita Berrada*

<br/>

|💡NOTES|
|:-------|
|1. You should only provide **code** if it adds anything to your answer: make sure your code confirms, reinforces, or complements your answers. Adding code just for the sake of it will not help you get a higher grade.<br/><br/> 2. You are free to choose the dataset on which you do the analysis to answer each question. **Only choose a single dataset per question**.<br/><br/> 3. Given that the datasets are large, you are allowed to use a subsample of the datasets. What matters is that the analysis made on the dataset serves as a proof-of-concept of your approach to the tasks we set you. If you choose to sample the original data, simply explain how and why you did the sampling and what effect you think it might have on your results down the line.<br/><br/>4. If you use any method not seen in the course, justify why you're using it and explain how it works.|

<br/>




⏲️ **Due Date**: Monday, April 14th, 5pm 

If you update your files on GitHub after this date without an authorised extension, you will receive a [late submission penalty](https://info.lse.ac.uk/current-students/services/assessment-and-results/exams/exam-discipline-and-academic-misconduct).

Did you have an extenuating circumstance and need an extension? Send an e-mail to 📧 [Kevin](mailto:DSI.ug@lse.ac.uk?subject=DS202W%20-%20W11p1%20Summative%20Extension)

<br/>

⚖️ **Assignment Weight**:

This assignment is worth 30% of your final grade in this course.



> 30%


<br/>


> [!IMPORTANT]
>## Do you know your CANDIDATE NUMBER? You will need it.
> _"Your candidate number is a unique five digit number that ensures that your work is marked anonymously. It is different to your student number and will change every year. **Candidate numbers can be accessed using LSE for You.**"_ 
> Source: [LSE](https://www.lse.ac.uk/accounting/study/New-Arrival/Exams-and-Assessments)



# 📝 Instructions

👉 Read it carefully, as some details might change from one assignment to another.

1. Go to our Slack workspace's `#announcements` channel to find a GitHub Classroom link. Do not share this link with anyone outside this course!

2. Click on the link, sign in to GitHub and then click on the green button `Accept this assignment`.

3. You will be redirected to a new private repository created just for you. The repository will be named `ds202w-2025-2024-spring-term-summative--yourusername`, where `yourusername` is your GitHub username. The repository will be private and will contain a `README.md` file with a copy of these instructions.

4. Recall what is your LSE CANDIDATE NUMBER. You will need it in the next step. 

5. Create your own `<CANDIDATE_NUMBER>.qmd` file with your answers, replacing the text `<CANDIDATE_NUMBER>` with your actual LSE number. 

You can create a `.qmd` file from a Jupyter notebook (i.e `.ipynb`) by going on the VSCode Terminal, making sure you are in the same directory as your Jupyter notebook (use the `pwd` to check which directory you're in and `cd` command to change directory if needed) and then typing the following command:

```zsh
quarto convert <CANDIDATE_NUMBER>.ipynb
```

where `<CANDIDATE_NUMBER>.ipynb` is the name of the Jupyter notebook you want to convert into `.qmd`

Also check out the [Quarto documentation](https://quarto.org/docs/tools/vscode-notebook.html) to better understand the conversion from `ipynb` to `qmd`.

And check out [this tutorial](https://education.launchcode.org/intro-to-professional-web-dev/appendices/terminal-commands/terminal-commands-tutorial.html) if you want to better understand the commands you can run on your VSCode terminal (e.g to change current directory).

You can also use the `.qmd` file you used in the W01 lab as a template. Just remove anything that is not relevant to this assignment. 

6. Then, replace whatever is between the `---` lines at the top of your newly created `.qmd` file with the following:

    ```yaml
    ---
    title: "DS202W - Spring Term Summative"
    author: <CANDIDATE_NUMBER>
    output: html
    self-contained: true
    jupyter: python3
    engine: jupyter
    editor:
      render-on-save: true
       preview: true
    ---
    ```

    Once again, replace the text `<CANDIDATE_NUMBER>` with your actual LSE CANDIDATE NUMBER. For example, if your candidate number is `12345`, then your `.qmd` file should start with:

    ```yaml
    ---
    title: "DS202W - Spring Term Summative"
    author: 12345
    output: html
    self-contained: true
    jupyter: python3
    engine: jupyter
    editor:
      render-on-save: true
      preview: true
    ---
    ```

8. Fill out the `.qmd` file with your answers. Use headers and code chunks to keep your work organised. This will make it easier for us to grade your work. Learn more about the basics of **markdown formatting** [here](https://quarto.org/docs/authoring/markdown-basics.html).
   
    - Use headers and code chunks to keep your work organised. This will make it easier for us to grade your work. Learn more about the basics of **markdown formatting** [here](https://quarto.org/docs/authoring/markdown-basics.html). 

9.  Use the `#help` channel on Slack liberally if you get stuck.

10. Once done, click on the `Render` button at the top of the `.qmd` file. This will create an `.html` file with the same name as your `.qmd` file. For example, if your `.qmd` file is named `12345.qmd`, then the `.html` file will be named `12345.html`. 

    - **If you added any code,** ensure your `.qmd` code is reproducible. hat is, if we were to restart VSCode and run your notebook from scratch, from top to the bottom, we would get the same results as you did (with the instructions you gave us!).

    - If you add code, please ensure your code confirms, reinforces, or complements your answers. Adding code just for the sake of it will not help you get a higher grade.

11. Push both files (i.e `.qmd` and rendered HTML) to your GitHub repository. You can push your changes as many times as you want before the deadline. We will only grade the last version of your assignment. Not sure how to use Git on your computer? You can always add the files via the GitHub web interface. You can also review the GitHub starter assignment (provided alongside the W04 formative) for a refresher on GitHub notions.

12. Read the section **How to get help and collaborate with others** at the end of this document.

## "What do I submit?"

You will submit two files:

- A Quarto markdown file with the following naming convention: `<CANDIDATE_NUMBER>.qmd`, where `<CANDIDATE_NUMBER>` is your candidate number. For example, if your candidate number is `12345`, then your file should be named `12345.qmd`.

- An HTML file render of the Quarto markdown file. To generate a render, the easiest way is to include these lines
  
```yaml
editor:
  render-on-save: true
  preview: true
```

in your `.qmd` header so that an HTML file[^1] is generated each time you preview your document (make sure you also have the Quarto extension installed in VSCode so that you do the preview by clicking on a button at the top right corner of the VSCode menu bar without having to use the Terminal!). Also, don't forget to add the line ```self-contained: true``` to your `.qmd` header, otherwise none of your plots will show!

Your `.qmd` header should look something like this:

```yaml
---
title: "✏️ Spring Term Summative"
author: <CANDIDATE_NUMBER>
format: html
self-contained: true
jupyter: python3
engine: jupyter
editor:
  render-on-save: true
  preview: true
---

```

You don't need to click to submit anything. Your assignment will be automatically submitted when you `commit` **AND** `push` your changes to GitHub. You can push your changes as many times as you want before the deadline. We will only grade the last version of your assignment. Not sure how to use Git on your computer? You can always add the files via the GitHub web interface.

# 🗄️ The Data

For this assignment, you have the choice between three datasets (you can either use the same dataset for both questions or use a different dataset for each question but if you do, stick to one **single dataset per question**).

*👉🏻 Note: In all cases, you should not store the data in your GitHub repository, as it will likely be too large for version control. Consider using a `.gitignore` file to exclude the data from your repository.*


## First dataset: EU Debates dataset

To download the dataset, simply use [this link](https://huggingface.co/datasets/coastalcph/eu_debates/blob/main/eu_debates.zip).

The EU Debates dataset (more fully described [here](https://huggingface.co/datasets/coastalcph/eu_debates)) is a corpus that contains of about 87000 individual speeches from the European Parliament from the period between 2009 to 2023. It was originally released by (Chalkidis and Brandl 2024). Note that older debate speeches are originally in English, while newer ones can be linguistically diverse across the 23 official EU languages but where this is the case, a translation to English is provided.

*🔍 Hint: If you need to detect non-English text, you can have a look at [this page](https://medium.com/data-science/4-python-libraries-to-detect-english-and-non-english-language-c82ad3efd430) for ways to do that.*

## Second dataset: Consumer Complaint Database from the Consumer Financial Protection Bureau (US)

Use the button below to download the cleaned up dataset:

<a href="https://drive.google.com/file/d/1BZin61TMz8l-q6CxKA-mYM0iuAucbBj1/view?usp=sharing" style="margin-left:2em;" download>
    <button class="button-61">Download complaints_reduced.zip </button>
</a>

<br/>

(If you have any issues with downloading this dataset with the button above, simply download the original dataset using [this link](https://www.consumerfinance.gov/data-research/consumer-complaints/#download-the-data) and remove all instances where the value of the `Consumer complaint narrative` column is missing.)


This is an extensive database of financial product and service complaints (with indications of companies responded to each of them) for the period between February 2019 and February 2025.
The original, non-cleaned up data can be found [here](https://www.consumerfinance.gov/data-research/consumer-complaints/#download-the-data): we removed all instances that did not contain the text content of a complaint from the original data.

*⚠️ The full dataset is very large: it contains over 2.6 million complaints (845MB compressed and 3.55 GB uncompressed).*

*🔍 Hint: You can perform operations directly on the zip file: to see how, take a look at [this page](https://www.geeksforgeeks.org/read-a-zipped-file-as-a-pandas-dataframe/)*

## Third dataset: Reddit Mental Health Dataset

To download the dataset, simply use [this link](https://www.kaggle.com/datasets/entenam/reddit-mental-health-dataset). To answer the questions, you are free to use either the labeled or raw data provided (or both).

This is a dataset curated from Reddit which encompasses a comprehensive collection of posts from five key subreddits focused on mental health: r/anxiety, r/depression, r/mentalhealth, r/suicidewatch, and r/lonely. The discussions on these subreddits focus on mental health issues. The dataset covers the period between January 2019 and August 2022.

More details on this dataset can be found [here](https://www.kaggle.com/datasets/entenam/reddit-mental-health-dataset) or in the original paper that made the dataset public (see (Rani, Ahmed, and Subramani 2024)).


# 📋 Your Task

**What do we need from you?**

## Context

While we provide data, we will not specify the insights we seek in some questions. Instead, we will task you with proposing your approach to the data. This mirrors real-world scenarios in data science and academic research, where you are often given a dataset and asked to derive insights or address a problem.

💡 **Remember**: if you write Python code, please ensure your code confirms, reinforces, or complements your answers and that it aligns with the style of code we practiced throughout the course. Adding code just for the sake of it will not help you get a higher grade.

## Part 1: Similarity (40 marks)

Suppose we want to calculate the similarity between the documents in the dataset you chose (i.e either of the EU debates, Consumer Complaints Database or Reddit Mental Health Subreddits dataset).

How would you approach this task? And what insights do you derive from this calculation? What do those similarities reveal about the "social" dynamics specific to your dataset?

### Dataset-Specific Guidance for Part 1:

1. EU Debates:
Analyze similarities between speeches from different political groups or countries and what they indicate about political positioning.
2. Consumer Complaints Database:
Compare complaints across financial products/companies and what these patterns suggest about consumer experiences.
3. Reddit Mental Health Subreddits:
Consider similarities between different mental health subreddit posts and what these patterns reveal about online mental health discourse.

## Part 2: Unsupervised Learning (60 marks)

Propose **one compelling research question** that a social scientist could investigate with the dataset of your choosing (i.e either of the EU debates, Consumer Complaints Database or Reddit Mental Health Subreddits dataset) using the _unsupervised learning methods_ covered in this course. How would you answer the research question with these methods, in particular:

> 1. Which dataset features would you use to answer your question? Why? Is there any feature engineering involved?
> 2. Which unsupervised learning methods would you use to answer your question? Why?
> 3. How would you interpret the results of your methods within the specific context of your dataset?
> 4. Do you see a supervised learning approach being feasible for your research question? Why? Why not?

<br/>

>[!NOTE]
> *Constraints under which you need to operate:*
> 1. It's fine to refine the model you propose to answer your research question but one of your models (original model or refinement(s)) needs to include textual features
> 2. The answer to your research question needs to feature at least two unsupervised learning techniques (aside from dimensionality reduction) but you don't need to - **and you shouldn't!**- try every single method and every single possible set of features either **(strike a judicious balance here)**!
> 3. If you can't run your models on the full dataset, demonstrating them on a slice of the data is fine.

<br/>  

# ✔️ How we will grade your work

A 70% or above score is considered a distinction in the typical LSE expectation. This means that you can expect to score around 70% if you provide adequate answers to all questions, in line with the learning outcomes of the course and the instructions provided.

Only if you go above and beyond what is asked of you _in a meaningful way_ will you get a higher score. Simply adding more code or text will not get you a higher score. You need to add unique insights or analyses to get a distinction - we cannot tell you what these are, but these should be things that make us go, "_wow, that's a great idea! I hadn't thought of that_".

Here is a rough rubric of how we will grade your answers. Note that the rigor of our marking varies with the expected difficulty of the question – this is reflected on the marking rubric.

## Part 1: Similarity (40 marks)

- **>29 marks:** Your response, besides being accurate, well-explained, effectively formatted, and surprisingly concise, surprised us positively. That is because you might have devised a custom similarity measure logically justified for your chosen dataset or because you actually coded it and provided excellent plots and a compelling interpretation of the results. Either the mathematical formulation you provided or the plots you created provided deep insights into the dynamics of the dataset.

- **29 marks:** Your response is well-structured and covers all the questions concisely. We see a well-justified process of calculating the similarity between the documents of your chosen dataset, or maybe you demonstrated it directly with well-documented code. You chose a similarity measure that is logical, focusing on its application to the dataset rather than its technical details (we already know how the distances work). Your approach to interpreting the results directly addresses the research question, avoiding generic explanations. The markdown formatting is also excellent.

- **23-28:** The response is accurate and relevant. Clearly, you have thought of how the similarity metrics apply to your chosen dataset specifically. However, there are minor slips in some areas. For instance, some justifications are vague or unclear, or the formatting could be improved.

- **17-22:** Your response missed some crucial details from the instructions. The answer is somewhat correct but lacks precision. It is not well-explained. The response is somewhat generic and could apply to any dataset. The markdown formatting is somewhat poor.

- **16 marks:** A pass. We recognise a few keywords, and although the response is somewhat correct, it lacks precision. The answer is so generic that it could apply to any dataset. Your response resembles language copy-pasted from the web or an AI chatbot.

- **<16 marks:** No answer was provided, or the response is very inadequate.

## Part 2: Unsupervised Learning (60 marks)

- **>43 marks:** Besides being correct, precise, greatly formatted, and well-explained, you provided code that confirms, reinforces, or complements your answers. The code aligns with the style of code we practiced throughout the course; your plots are beyond the basic ones, and you provided a compelling interpretation of the results. 

- **43 marks:** Your response is well-structured and covers all the questions concisely. We see a clear explanation of how the question you're asking fits an unsupervised learning model rather than a supervised learning one and what you would do differently process-wise from the supervised learning case (maybe you even included snippets of code to showcase the difference). The choice of algorithm is logical, focusing on its application to the dataset you chose rather than its technical details (we already know how the algorithms work). Your approach to interpreting the results directly addresses the research question, avoiding generic explanations. The markdown formatting is also excellent.

- **34-42:** The response is accurate and relevant. Clearly, you know in which cases you should apply unsupervised learning techniques to the dataset you chose and have come up with interesting questions to match. However, there are minor slips in some areas. For instance, you might have missed a minor step your explanations of the process, some justifications are vague or unclear, or the formatting could be improved.

- **25-33:** Your response missed some crucial details from the instructions. The answer is somewhat correct but lacks precision. It is not well-explained. The response is somewhat generic and could apply to any dataset. The markdown formatting is somewhat poor.

- **24 marks:** A pass. The response is somewhat correct but it lacks precision. The answer is so generic that it could apply to any dataset and doesn't offer any real insights. Your response resembles language copy-pasted from the web or an AI chatbot.

- **<24 marks:** No answer was provided, or the response is very inadequate or irrelevant to the question.


# How to get help and collaborate with others


## 🙋 Getting help

You can post general clarifying questions on Slack.

For example, you can ask:

- _"Where do I find material that compares different clustering techniques?"_
- _"I came across the term 'loadings' when reading about PCA in the textbook, but I don't fully understand it. Does anyone have a good alternative resource about it?"_

You won't be penalized for posting something on Slack that violates this principle without realizing it. Don't worry; we will delete your message and let you know.

## 👯 Collaborating with others

You are allowed to discuss the assignment with others, work alongside each other, and help each other. However, you cannot share or copy code from others — pretty much the same rules as above.

## 🤖 Using AI help?

You can use Generative AI tools such as ChatGPT when doing this research and search online for help. If you use it, however minimal use you made, you are asked to report the AI tool you used and add an extra section to your notebook to explain how much you used it.

Note that while these tools can be helpful, they tend to generate responses that sound convincing but are not necessarily correct. Another problem is that they tend to create formulaic and repetitive responses, thus limiting your chances of getting a high mark. When it comes to coding, these tools tend to generate code that is not very efficient or old and does not follow the principles we teach in this course.

To see examples of how to report the use of AI tools, see 🤖 [Our Generative AI policy](https://lse-dsi.github.io/DS202/2024/winter-term/generative-ai.qmd).

## References 

Chalkidis, Ilias, and Stephanie Brandl. 2024. “Llama Meets EU: Investigating the European Political Spectrum Through the Lens of LLMs.” arXiv Preprint arXiv:2403.13592. [https://arxiv.org/abs/2403.13592](https://arxiv.org/abs/2403.13592).

Rani, Saima, Khandakar Ahmed, and Sudha Subramani. 2024. “From Posts to Knowledge: Annotating a Pandemic-Era Reddit Dataset to Navigate Mental Health Narratives.” Applied Sciences 14 (4). [https://doi.org/10.3390/app14041547](https://doi.org/10.3390/app14041547).

[^1]: it will normally be saved in the same folder as your `.qmd` and have the same name as the `.qmd`, e.g if the `.qmd` is called `12345.qmd`, the HTML will be called `12345.html`





