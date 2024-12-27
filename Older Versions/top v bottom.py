
import pandas as pd

data = pd.read_csv("/Users/maia/Library/CloudStorage/OneDrive-UW/CCDL/MGH Research Project/CardSort_Summary(in).csv")

df = pd.DataFrame(data)

# Define a function to determine response type
def determine_response(row):
    parts = row['Trial Info'].split('_')
    top_stim = parts[1]  # 'Sym' or 'Txt'
    top_suit = parts[2]  # 'Heart', 'Spade', or 'Club'
    bottom_stim = parts[3]  # 'Sym' or 'Txt'
    bottom_suit = parts[4]  # 'Heart', 'Spade', or 'Club'
    
    suit_to_response = {'Club': 1, 'Heart': 2, 'Spade': 3}
    top_response = suit_to_response.get(top_suit)
    bottom_response = suit_to_response.get(bottom_suit)
    
    if row['SubjectResp'] == top_response:
        return 'top'
    elif row['SubjectResp'] == bottom_response:
        return 'bottom'
    else:
        return 'incorrect'

# Apply the function to each row
data['Response Type'] = data.apply(determine_response, axis=1)

# Display the updated DataFrame
print(data[['Trial Info', 'SubjectResp', 'Response Type']])
