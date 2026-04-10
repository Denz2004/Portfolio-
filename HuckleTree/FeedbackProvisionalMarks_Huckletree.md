# Huckletree

## Project title: Huckletree
## Group: 42365 40294 39109

## ## TOPIC AND SCENARIO (maximum 15 points)

Very interesting topic. Relevant and realistic scenario. Well motivated and rich data, and the goal to support investor decision-making. Focusing on unicorn companies makes sense given the size of the data. The integration of vector search in MongoDB is a standout feature of the project.

## ## DATA (maximum 10 points)

Good overall. Some parts are not clear in the report. Is the dataset sourced from Runa Capital itself a dataset that aggregates the data from crunchbase, dealroom, linkedin. Some comments missing here for transparency. Providing a small sample or schema of the CSVs would improve. Some missing values mention (are those the only missing values). Did the data require any preprocessing some comments missing for completeness. Good that you showed one instance from each collection!

## ## DATABASE MODELLING (maximum 20 points)

While the data modelling follows general NoSQL practices—collections for entities, referencing via unique keys (e.g., uuid, cb_uuid, domain)—the actual implementation appears to closely mimic a relational database schema. All the data comes from flat CSVs, and the processing maintains that flatness when transferring to MongoDB. There is no evidence of nested documents or arrays, which are some of the core design advantages of MongoDB.

In MongoDB, one of the biggest benefits is being able to store related data directly **within** a parent document when that relationship is tight or hierarchical. This improves performance and avoids the need for frequent $lookup joins.

Perhaps founders could be embedded within each unicorn document. Github repositories as well.

## ## DATABASE CREATION (maximum 10 points)

Some comments from the previous part remain here. Otherwise overall looks good!
- Great that you used vector embeddings and vector indexes. What about indexes on some other fields - perhaps commonly joined fields (if using lookup frequently creating indexes on both sides can improve performance).
- There is no schema (perhaps for some fields you do want to ensure correctness). You could have created the schema before inserting.

## ## DATABASE USAGE (maximum 35 points)

- **Query 1** - simple data retrieval. Performance could be improved by embedding frequently accessed subdocuments (e.g., founders) directly within the unicorn documents, reducing the need for multiple lookups
- **Query 2**- simple but clear query, good use of geo data and visualisation (the hover data could be formatted better)
- **Query 3** - Good, includes grouping and has time component. There could be more comments on the output (trying to draw some conclusions). Plotly animation is a nice touch,  but perhaps it would be better if the colour scale stayed the same across years (it recalibrates every year)
- **Query 4** - Good. It is noted that companies can belong to multiple industries (stored in a list), but is this then a limitation of the query, that is not discussed. Is it assumed that each company that belongs to multiple industries contributes full funding to multiple categories? Plotly visualisation showing all industries is not good - the colour palette is repeating and it is not possible to distinguish between the industries. There is no comment or the analysis of the obtained results
- **Query 5** - Practical and relevant query. A limitation is that only 2024 is considered, maybe this is not reliable for all companies then (some might have a weird year). Perhaps you could have also grouped this by different industries. Again, some discussion of the results would strengthen the quality.
- **Query 6 and 7** - Very good, advanced use of MongoDB using vector search, and interesting questions! However, again no analysis of the results

## ## DATABASE TECHNOLOGY (maximum 5 points)

The choice of MongoDB is appropriate for this project, particularly given the use of vector search capabilities.  However, the database model itself largely mimics a relational structure, relying on referencing and $lookup joins rather than leveraging MongoDB’s strengths like embedded documents, arrays, or hierarchical data.

## ## DOCUMENTATION (maximum 5 points)

Overall good, could be better formatted, some outputs included as screenshots are not readable,


| Problem breakdown       | Max marks | Your marks |
|-------------------------|-----------|------------|
| (2) Topic/scenario      | 15        | 12         |
| (3) Data                | 10        |  5         |
| (4) Database modelling  | 20        | 12         |
| (5) Database creation   | 10        |  5         |
| (6) Database usage      | 35        | 28         |
| (7) Database technology | 5         |  3         |
| Documentation           | 5         |  3         |
| TOTAL                   | 100       | 68         |
