verilog-mode verilog-ts-mode

(if "if (" p ") begin" n> q n "end">)
(case "case (" p ")" n> "default: " q ";" n> "endcase">)
(module "module " (s "name") ";" n> q n "endmodule">)
(for "for (" p ") begin" n> q n "end">)

;; Data types
(struct "struct {" n> p n "} " (s name) ";"> q)
;;(interface "interface

;; Behavioral blocks
(begin "begin" n> q n "end">)
(always "always @(" p ") begin" n> q n "end">)
(always_ff "always_ff @(" p ") begin" n> q n "end">)
(always_latch "always_latch begin" n> q n "end">)
(comb "always_comb begin" n> q n> "end">)
(initial "initial begin" n> q n> "end">)
(assign "assign " p " = " q ";")

;; System Verilog features
(class "class " p ";" n> q n "endclass">)
(task "task " p "(" p ");" n> q n "endtask">)
(function "function " p "(" p ");" n> q n "endfunction">)
(generate "generate " n> q n> "endgenerate">)
(package "package " p ";" n> q n> "endpackage">)

;; (assert "assert (" p ") begin" n> "$display(\"" p "\");" n "end else begin" >n> q n "end">)
