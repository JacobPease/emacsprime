verilog-mode verilog-ts-mode

(module "module " (s "name") ";" n> q n "endmodule">)

(ff "always_ff @(" p ") begin" n> q n> "end">)

(comb "always_comb begin" n> q n> "end">)

(initial "initial begin" n> q n> "end">)

(assign "assign " p " = " q ";")

(class "class " p ";" n> q n "endclass">)

(task "task " p "(" p ");" n> q n "endtask">)

(function "function " p "(" p ");" n> q n "endfunction">)

(generate "generate " n> q n> "endgenerate">)

(package "package " p ";" n> q n> "endpackage">)

(case "case (" p ")" n> "default: " q ";" n> "endcase">)

(begin "begin" n> q n "end">)

;; (assert "assert (" p ") begin" n> "$display(\"" p "\");" n "end else begin" >n> q n "end">)
