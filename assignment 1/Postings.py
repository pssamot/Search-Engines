
class Postings:

    def __init__(self, term, document, indexInDocument):
        self.term = term

        self.documentsIn = []
        self.documentsIn.append(document)

        self.termFrequency = 1
        self.termDocumentFrequency = 1

        self.indexInDocument = dict()
        self.indexInDocument[document] = []
        self.indexInDocument[document].append(indexInDocument)

        self.td_idf_inDocument = dict()
        self.td_idf_inDocument[document] = 0



    
    def foundNewTerm(self, document, indexInDocument):
        self.termFrequency += 1
        # found in a different document
        if document not in self.indexInDocument: 
            self.termDocumentFrequency += 1
            self.indexInDocument[document] = []
        self.indexInDocument[document].append(indexInDocument)

    def __repr__(self):
        #printable = self.term + " tf " + str(self.termFrequency) + "tdf" + str(self.termDocumentFrequency)
        printable = self.term + " -> "
        aux = ''
        for doc, indexes in self.indexInDocument.items():
            indexesToString =":".join(str(x) for x in indexes)
            aux += "| D"+str(doc) + " "+  indexesToString + "\n"

        return printable + aux
    def __str__(self):
        # printable = self.term + " tf " + \
        #     str(self.termFrequency) + " tdf " + str(self.termDocumentFrequency)
        printable = self.term + " -> "
        aux = ''
        for doc, indexes in self.indexInDocument.items():
            indexesToString = ":".join(str(x) for x in indexes)
            aux += "| D"+str(doc) + " " + indexesToString 
        return printable + aux
