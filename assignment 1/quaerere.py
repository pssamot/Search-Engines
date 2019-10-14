import string
import math
import re
from Postings import Postings

number_documents = 0

def updateTF_IDF(documents, documentsTerms):
    global number_documents

    for __, info in documentsTerms.items():
        for doc, __ in info.indexInDocument.items():
            info.td_idf_inDocument[doc] = (
                len(info.indexInDocument[doc]) / len(documents[doc])) * math.log(number_documents / info.termDocumentFrequency)

def createPostings(documents):
    terms = dict()
    for docId, document in enumerate(documents):
        for termIndex, word in enumerate(document):
            if word not in terms:
                terms[word] = Postings(word, docId, termIndex)
            else:
                terms[word].foundNewTerm(docId, termIndex)
    return terms

def processDocuments():
    global number_documents
    with open("collection-100.txt", "r") as paragraphs_file:
        documents = []
        fullDocuments = []
        for paragraph in paragraphs_file:  # one line until the end of file
            if paragraph != "\n":
                fullDocuments.append(paragraph)
                #take \n
                paragraph = paragraph.strip('\n')
                #to lower
                paragraph = paragraph.lower()
                #take out punctuation
                paragraph = paragraph.translate(
                    str.maketrans('', '', string.punctuation))
                #take out the space
                paragraph = paragraph.split()
                #remove words less 4 letters
                paragraph = list(filter(lambda x: len(x) >= 4, paragraph))
                #take out the s in the end
                paragraph = [word[:-1] if word[-1] == 's' or word[-1]
                    == 'S' else word for word in paragraph]

                documents.append(paragraph)
                number_documents += 1
        return documents, fullDocuments
    return -1


def processQuery(query, documentsTerms, documents):

    queryTerms = query.split()
    documentsTermAppeared = []

    term_weight = 1/len(queryTerms)

    if len(queryTerms) == 1:
        #Calculate the td_
        docs = {doc: documentsTerms[queryTerms[0]].td_idf_inDocument[doc] for doc in documentsTerms[queryTerms[0]].indexInDocument.keys()}
       
    else:
        #Get the documents and indexes where the term appeared
        for queryTerm in queryTerms:
            if queryTerm in documentsTerms:
                documentsTermAppeared.append(
                    documentsTerms[queryTerm])

        #Intersects the lists of docs
        docs = dict()
        for i in range(len(documentsTermAppeared)):
            for doc in documentsTermAppeared[i].indexInDocument.keys():
                if doc not in docs:
                    score_doc = documentsTermAppeared[i].td_idf_inDocument[doc]
                    for j in range(len(documentsTermAppeared)):
                        if i != j:
                            if doc in documentsTermAppeared[j].indexInDocument:
                                score_doc += documentsTermAppeared[j].td_idf_inDocument[doc]

                    #assume every term has the same weight in the query
                    # the term weight in the doc is computed with td_idf
                    similarity = score_doc * term_weight
                    docLenght = len(documents)
                    queryLen = len(queryTerms)
                    cosine = similarity / (math.sqrt(docLenght) * math.sqrt(queryLen))
                    docs[doc] = 100*cosine


    docs = dict(sorted(docs.items(), key=lambda x: x[1], reverse=True))
    return docs


def createDocListings(documents, documentsTerms):
    docsPostings = dict()
    for docId, doc in enumerate(documents):
        docsPostings[docId] = []
        for key,term in documentsTerms.items():
            if docId in term.indexInDocument:
                docsPostings[docId].append(term)
    return docsPostings
def computeL2(query, wordsInDocument, docId):

    queryTerms = query.split()
    term_weight = 1/len(queryTerms)
    l2 = 0
    for word in wordsInDocument:
        if word.term in queryTerms:
            l2 += (word.td_idf_inDocument[docId] *
                   term_weight - word.td_idf_inDocument[docId])**2
        else:
            l2 += (- word.td_idf_inDocument[docId])**2

    return math.sqrt(l2)

def funcOrderWords(obj, doc):
    return obj.td_idf_inDocument[doc]


def printResults(docs, documentsTerms):
    top_words = []
    counter = 0
    for doc in docs.keys():
        top_words = []
        uniqueWords = 0
        #Doc ID
        print("DID " + str(doc))

        for __, term in documentsTerms.items():
            if doc in term.indexInDocument:
                top_words.append(term)
                if len(term.indexInDocument) == 1:
                    uniqueWords += 1
        #Top 5 words            
        toPrint = list(sorted(top_words, key=lambda x: funcOrderWords(x, doc), reverse=True))
        for p in toPrint[:5]:
            print(p)
        
        print("Number of unique keywords in document: " + str(uniqueWords) )
        print("Cosine Similarity %.3f " % (docs[doc]))
        print("Magnitude of the document vector (L2 norm): %.3f \n" %
              (computeL2(query, docsPostings[doc], doc)))
        counter += 1
        if counter > 2:
            break

if __name__ == "__main__":

    print("\n")

    documents, fullDocuments  = processDocuments()
    documentsTerms = createPostings(documents)
    
    updateTF_IDF(documents, documentsTerms)
    docsPostings = createDocListings(documents, documentsTerms)

    with open("query-10.txt") as f:
        for line in f.readlines():
            query = re.sub(r"[\n\",@\'?\.$%_]", "", line, flags=re.I)
            query = query.lower()
            re.sub("[^a-zA-Z]+", "", query)
            print("-- Results query: " + query + " --")
            docs = processQuery(query, documentsTerms, documents)
            printResults(docs, documentsTerms)
            print("-- end --")
    print("\n")


