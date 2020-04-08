# The Goa Government Door-to-door Community Health Survey (also known as "Goa Covid Health Survey")

## Important Caveat

Please note this is **my personal account** about the happenings before, during, and after the Goa Covid Survey. I am reasonably certain that if any government official is reached out for official comments, they will deny, deflect, or claim ignorance about uncomfortable questions. As I have learnt during this exercise, transparency doesn't come naturally to anyone in the government machinery.

## Table of Contents

- Important caveats

- How did this all start?

- What was the survey about? 

  - Survey questions

- The technology
  
  - Trying to find a quick technology solution

  - Learnings from the pilot survey
  
  - Putting together a working technlogy solution
  
- The people

- Scaling up training and communication

  - Recording the video
  
  - Training

- Comments / learnings

  - Comments/learnings about the process and working alonside government machinery

  - Comments/learnings about Zoho Forms

  - Comments/learnings about Haskell

  - Comments/learnings about Data privacy

- Results and impact

## How did this start?

I am made to believe that the genesis of this massive exercise was the so-called "Bhilwara model". We knew about it 7-10 days _before_ it was trending on Twitter. Strangely, when it was trending on Twitter, the only thing being talked about, was the "ruthless containment". However, there was a also a door-to-door survey conducted across **22 days** throughout the district of Bhilwara, which the Government of Goa wanted to replicate. 

That is where this idea started.

Also, I do not think that Bhilwara used technology to rapidly conduct the survey and collate results. It was a paper-based survey, and I do not know how they collated this data from lakhs of paper sheets. 

I was called-upon for help by an IAS officer (who shall remained unnamed) who was aghast that the initial idea was to replicate this **paper-based** survey. 

## What was the survey about?

The Department of Health (Govt of Goa), wanted to primarily collect data about:

- people (or population clusters) with ILI symptoms (influenze-like illness)
- people (or population clusters) in other high-risk categories (i.e. elderly people, existing chronic illnesses, recent travel history, etc)

### Questionnaire

After a bunch of iterations, and navigating [HiPPOs](https://whatis.techtarget.com/definition/HiPPOs-highest-paid-persons-opinions), the final set of survey questions is given below:

- Name of head of houshold
- Phone number of head of household
- Is anyone in the group showing flu-like symptoms, including fever, cough, and difficulty in breathing?
- Has anyone in the group come in contact with any other person showing the above symptoms?
- Has anyone in the group come to Goa after 15 Feb, 2020?
- Has anyone in the group come in contact with any person who has come to Goa after 15 Feb, 2020? 
- Anyone in the group with diabetes?
- Anyone in the group with heart disease?
- Anyone in the group with high blood pressure?
- Anyone in the group with kidney disease?
- Anyone in the group with respiratory disease?
- Ages of members in the family
