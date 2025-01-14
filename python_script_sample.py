
import subprocess


result = subprocess.run(
    ["Rscript", "your_script.R"],
    capture_output=True,
    text=True
)
