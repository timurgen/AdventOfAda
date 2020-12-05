with open('input.txt', 'r') as f:
    seat_list = f.readlines()

def parse_seat(s: str):
    s = s.strip('\n')
    # rows
    row_low = 0
    row_high = 127
    for idx in s[:6]:
        if idx == 'F':  # keep lower half
            row_high = (row_high + row_low) // 2
        elif idx == 'B':  # keep upper half
            row_low = round((row_high + row_low) / 2)
    # cols
    col_low = 0
    col_high = 7
    for idx in s[7:9]:
        if idx == 'L':  # keep lower half
            col_high = (col_high + col_low) // 2
        elif idx == 'R':  # keep upper half
            col_low = round((col_high + col_low) / 2)
    return row_high if s[6] == 'B' else row_low, col_high if s[9] == 'R' else col_low

highest_id = 0
ids = []
for seat in seat_list:
    row, column = parse_seat(seat)
    id = row * 8 + column
    if id > highest_id:
        highest_id = id
    ids.append(row * 8 + column)
print(highest_id)
ids.sort()
for i in range(1, len(ids)):
    if ids[i] - ids[i - 1] > 1:
        print(ids[i] - 1)
