import subprocess
import argparse

# Change this to the cell lib for the required technology
std_cell_lib="/Users/karthisrinivasan/Documents/act_tools/act/syn/liberty/osu018_stdcells.lib"

def xcell_char(tech, verbose):
    process = subprocess.Popen(
        ["xcell", f"-T{tech}", "char.act", "out"],            
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True            # text=True => use strings (instead of bytes)
    )
    stdout, stderr = process.communicate()
    
    if (verbose):
        print("[STDOUT]")
        print(stdout)
        print("[STDERR]")
        print(stderr)


def std_cell_char(file):
    process = subprocess.Popen(
    ["abc"],            
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True            # text=True => use strings (instead of bytes)
    )

    commands = f"""read_lib -v {std_cell_lib}
    read_constr constr.sdc
    read_verilog -m {file}
    stime -p -d
    """

    for command in commands:
        process.stdin.write(command)
        process.stdin.flush()

    stdout, stderr = process.communicate()

    print("[STDOUT]")
    print(stdout)
    print("[STDERR]")
    print(stderr)


def abc_time (file, verbose):
    process = subprocess.Popen(
        ["abc"],            
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True            # text=True => use strings (instead of bytes)
    )

    commands = f"""read_lib -v out.lib
    read_constr constr.sdc
    read_verilog -m {file}
    stime -p -d
    """

    for command in commands:
        process.stdin.write(command)
        process.stdin.flush()

    stdout, stderr = process.communicate()

    if (verbose):
        print("[STDOUT]")
        print(stdout)
        print("[STDERR]")
        print(stderr)


delays = {}
def read_stats (verbose):
    with open("stats.txt") as f:
        data = [line.split() for line in f if line.strip()]
        for d in data:
            delays[d[0]] = d[-1]
        if (verbose):
            print(delays)


cells = ["invx1", "latchlo", "or2", "or3", "mux2", "mux3"]
# cells = ["invx1", "latchlo", "or2", "or3", "or4", "or5", "or6", "or7", "or8", "mux2", "mux3", "mux4", "mux5", "mux6", "mux7", "mux8"]
vfiles = ["invt", "latch_t", "or_2", "or_3", "mux_2", "mux_3"]
# vfiles = ["invt", "latch_t", "or_2", "or_3", "or_4", "or_5", "or_6", "or_7", "or_8", "mux_2", "mux_3", "mux_4", "mux_5", "mux_6", "mux_7", "mux_8"]


def get_delay (k):
    if k in delays:
        return float(delays[k])
    else:
        return 2000.0


def write_conf (file, tech):
    with open(file, "w") as file_object:
        file_object.write(f"#------------------------------------------------\n")
        file_object.write(f"# Synthesis Configuration File - {tech}\n")
        file_object.write(f"#------------------------------------------------\n")
        file_object.write(f"begin synth \n")
        file_object.write(f" \n")
        file_object.write(f"    begin ring \n")
        file_object.write(f" \n")
        file_object.write(f"        # Verbose ring synthesis output \n")
        file_object.write(f"        int verbose 0 \n")
        file_object.write(f" \n")
        file_object.write(f"        # NOTE:  \n")
        file_object.write(f"        # The appropriate values for parameters here are \n")
        file_object.write(f"        # technology-specific and hence will have to \n")
        file_object.write(f"        # be updated when a new technology is used. \n")
        file_object.write(f"        begin bundled \n")
        file_object.write(f" \n")
        file_object.write(f"            # Number of different delay line types \n")
        file_object.write(f"            int delay_line_types 4 \n")
        file_object.write(f" \n")
        file_object.write(f"            # Define a continuous monotonically  \n")
        file_object.write(f"            # increasing piece-wise linear  \n")
        file_object.write(f"            # curve for delay vs. param (N), \n")
        file_object.write(f"            # as (x,y) pairs. \n")
        file_object.write(f"            # Last pair is typically two large values \n")
        file_object.write(f"            # corresponding to (inf,inf), \n")
        file_object.write(f"            # this is the largest allowed delay line \n")
        file_object.write(f" \n")
        file_object.write(f"            # delay_params is x-axis (N) \n")
        file_object.write(f"            int_table delay_params 0 5 10 50 10000 \n")
        file_object.write(f"             \n")
        file_object.write(f"            # delay_vals is y-axis (ps) \n")

        d = 2*float(delays["invt"])

        file_object.write(f"            real_table delay_vals 0.0 {5*d} {10*d} {50*d} {10000*d} \n")
        file_object.write(f" \n")
        file_object.write(f"            # Datapath delay values \n")
        file_object.write(f" \n")
        file_object.write(f"            # Pulse width of pulse generator (ps) \n")

        pw = float(delays["latch_t"])

        file_object.write(f"            real pulse_width {pw} \n")
        file_object.write(f" \n")
        file_object.write(f"            # Capture delay of a single latch (ps) \n")
        file_object.write(f"            # FF is essentially two of these in series \n")

        cd = float(delays["latch_t"])

        file_object.write(f"            real capture_delay {cd} \n")
        file_object.write(f" \n")
        file_object.write(f"            # Merge-mux delays (ps): \n")
        file_object.write(f"            # Last value will be used if mux larger  \n")
        file_object.write(f"            # than max-size defined here \n")
        file_object.write(f"            # inputs:                 2      3      4      5      6      7      8     9+ \n")
        file_object.write(f"            real_table mux_delays {get_delay("mux_2")} {get_delay("mux_3")} {get_delay("mux_4")} {get_delay("mux_5")} {get_delay("mux_6")} {get_delay("mux_7")} {get_delay("mux_8")} {get_delay("mux_9")} \n")
        file_object.write(f" \n")
        file_object.write(f"            # OR-gate delays (ps): \n")
        file_object.write(f"            # Last value will be used if OR-gate larger  \n")
        file_object.write(f"            # than max-size defined here \n")
        file_object.write(f"            # inputs:              0   1     2     3     4      5      6      7      8    9+ \n")
        file_object.write(f"            real_table or_delays 0.0 0.0 {get_delay("or_2")} {get_delay("or_3")} {get_delay("or_4")} {get_delay("or_5")} {get_delay("or_6")} {get_delay("or_7")} {get_delay("or_8")} {get_delay("or_9")} \n")
        file_object.write(f" \n")
        file_object.write(f"            # These are for latency cost calculation for decomp - will be derived from timer \n")
        file_object.write(f"            real send_delay 40.0 \n")
        file_object.write(f"            real recv_delay 40.0 \n")
        file_object.write(f"            real assn_delay 20.0 \n")
        file_object.write(f" \n")
        file_object.write(f"            # way:                  0   1    2    3    4    5    6    7    8   9+ \n")
        file_object.write(f"            real_table sel_delays 0.0 0.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0 90.0 \n")
        file_object.write(f" \n")
        file_object.write(f"        end \n")
        file_object.write(f" \n")
        file_object.write(f"    end \n")
        file_object.write(f" \n")
        file_object.write(f"end \n")


def run(tech, verbose):

    print("Running xcell...\n")
    xcell_char(tech, verbose)

    for f in vfiles:
        print(f"Timing cell: {f}\n")
        abc_time(f"verilog/{f}.v", verbose)

    read_stats(verbose)

    print("Writing conf file...\n")
    write_conf(f"synth_{tech}.conf", tech)


parser = argparse.ArgumentParser(
    description="Generate Synthesis Configuration File for a Technology"
)
parser.add_argument("-T", "--tech", required=True)
parser.add_argument("-v", "--verbose", required=False, action="store_true")
args = parser.parse_args()

run(args.tech, args.verbose)