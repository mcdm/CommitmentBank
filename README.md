# CommitmentBank

The repository contains the materials related to the paper:

Marie-Catherine de Marneffe, Mandy Simons, and Judith Tonhauser (2019). The CommitmentBank: Investigating projection in naturally occurring discourse. Proceedings of Sinn und Bedeutung 23.

# Data
The CommitmentBank is a corpus of 1,200 naturally occurring discourses whose final sentence contains a clause-embedding predicate under an entailment canceling operator (question, modal, negation, antecedent of conditional).

<strong>CommitmentBank-items.csv</strong> contains the data, one item per row

<strong>CommitmentBank-All.csv</strong> contains each participant's answer

- Target: sentence of interest, containing a clause-embedding predicate under an entailment canceling operator
- Context: preceding context of the target sentence (up to 2 sentences/turns)
- Prompt: prompt used in the experiment to gather projection judgments
- Verb: clause-embedding predicate
- Embedding: type of entailment canceling operator 
- factive: whether the verb is canonically considered factive or not
- ModalType: for modal embedding, type of modal (AB: ability, CI: circumstancial, DE: deontic, EP: epistemic)
- MatTense: tense of the matrix verb
- MatSubjLemma: lemma of the matrix verb subject
- MatSubjPer: person of the matrix verb subject
- MatSubjNum: number of the matrix verb subject
- genre: corpus from which the item has been extracted
- Answer/Responses: projection judgments to the prompt, using a 7-point Likert scale (-3/the author is certain that the prompt is false, 0/the author is not certain whether the prompt is true or false, 3/the author is certain that the prompt is true)
- mean.noTarget: mean of responses to the "plausibility of the content of the complement given context" (see Section 2.6 in the paper)
- sd.noTarget: standard deviation of responses to the "plausibility of the content of the complement given context" (see Section 2.6 in the paper)

# Code
<strong>analysis-SuB-paper.R</strong> contains the R code to reproduce the figures and models in the paper. At time of publication, a few items were missing annotation for the tense of the matrix verb. These got fixed in the released data. However running the code on the released data leads to minimal discrepancies from what is reported in the paper wrt tense.

# License
Creative Commons Attribution-Non-Commercial-ShareAlike 4.0 Internal License
