# home-credit-project

Kayli Thompson

## Business Problem
Home Credit offers loans to customer segments typically overlooked by traditional lenders due to lower credit scores. Instead of relying solely on credit scores, Home Credit evaluates a broader set of applicant features to assess the likelihood of loan default.

## Project Objective
The goal of this project is to develop and optimize a predictive model that estimates the probability of a client defaulting on a loan, using the diverse features provided in the Home Credit dataset. This enables more inclusive, data-driven lending decisions while managing risk.

## Solution to Business Problem
To address the challenge of lending to customers with limited or low traditional credit scores, we developed a machine learning solution using a LightGBM model. This model analyzes a wide range of applicant features—including demographics, employment, income, and external credit sources—to estimate each applicant’s probability of default.

Crucially, instead of using a fixed cutoff, the decision threshold for loan approval is set individually for each applicant based on the expected profitability of their loan. A loan is approved only if the model predicts that the expected profit (from interest and fees) exceeds the expected loss (from potential default), given the applicant’s profile and loan terms. This approach enables Home Credit to make more inclusive, data-driven lending decisions that maximize expected profit while managing risk, going beyond what traditional score-based methods allow.

## My Contribution to the Project
* Developed the final LightGBM model, which achieved the highest predictive performance in the group and was selected as the team’s production model.
* Designed and implemented the profitability analysis, quantifying the business impact of model-driven decisions.
* Created key charts and drafted the outline for the final presentation.
* Coordinated team meetings and managed the project schedule to ensure timely progress and collaboration.

## Business Value of Solution
By implementing this LightGBM-based decision system, Home Credit can make more profitable and inclusive lending decisions. The model enables the company to approve loans for applicants who might otherwise be declined by traditional credit scoring, while still managing risk through a profitability-based threshold for each loan.

On the validation dataset, this approach is estimated to save approximately $2.18 million for every 1,000 loan applications compared to a baseline of approving all applicants. These savings result from more accurately identifying high-risk applicants and reducing losses from defaults, while still capturing revenue from safe loans. At scale, this translates to substantial financial impact and improved portfolio performance for the business.

## Difficulties Faced
Our group encountered several challenges during this project:

* Domain Knowledge: Most team members were not familiar with the loan approval process or industry-specific terminology. We needed to invest time in understanding how lending decisions are made and what factors drive profitability and risk in consumer credit.
* Modeling Techniques: Many of us had limited experience with advanced machine learning models, including LightGBM. We had to learn how to implement, tune, and interpret these models effectively.
* AI Integration: This was our first project using AI tools to support model development, documentation, and analysis. We had to quickly adapt to new workflows and best practices for leveraging AI in a data science project.

Despite these difficulties, the team collaborated to overcome knowledge gaps and deliver a robust, business-focused solution.

## What I Learned
The most valuable lesson from this project was how to effectively integrate AI into a real-world data science workflow. I learned that it’s essential to actively drive the session with AI—providing clear direction, setting boundaries, and remaining in control—rather than expecting the AI to independently deliver a complete solution from a set of requirements.

Throughout the project, I found that I needed to step back and consider the bigger picture, redirecting the AI when it pursued overly complex or inefficient paths. When the AI-generated code produced errors or failed to meet requirements, I learned to adjust my instructions and parameters to guide it toward a better outcome.

Most importantly, I realized that the value of AI-generated work depends on my own understanding. If I can’t explain the solution to stakeholders in my own words, the AI’s output is of limited use. This project reinforced the importance of combining AI assistance with critical thinking and clear communication.

