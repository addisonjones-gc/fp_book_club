main :: IO()
toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ "\n"
fromPart author = "Thanks, \n" ++ author
createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author
main = do
    print "Who is the email for?"
    recipient <- getLine 
    print "What is the Title?"
    title <- getLine 
    print "Who is the author?"
    author <- getLine 
    print (createEmail recipient title author)
