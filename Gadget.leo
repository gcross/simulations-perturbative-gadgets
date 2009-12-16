<?xml version="1.0" encoding="utf-8"?>
<?xml-stylesheet ekr_test?>
<leo_file>
<leo_header file_format="2" tnodes="0" max_tnode_index="0" clone_windows="0"/>
<globals body_outline_ratio="0.5">
	<global_window_position top="50" left="990" height="1168" width="925"/>
	<global_log_window_position top="0" left="0" height="0" width="0"/>
</globals>
<preferences/>
<find_panel_settings/>
<vnodes>
<v t="gcross.20091211100630.1232" str_leo_pos="3"><vh>@chapters</vh></v>
<v t="gcross.20091211100630.1234"><vh>@thin Makefile</vh></v>
<v t="gcross.20091211140008.1277"><vh>Models</vh>
<v t="gcross.20091211140008.1278"><vh>#1</vh></v>
<v t="gcross.20091211140008.1279"><vh>#2</vh></v>
<v t="gcross.20091211140008.1280"><vh>#3</vh></v>
<v t="gcross.20091211140008.1281"><vh>#4</vh></v>
<v t="gcross.20091211140008.1282"><vh>#5</vh></v>
<v t="gcross.20091211140008.1283"><vh>#6</vh></v>
<v t="gcross.20091211140008.1284"><vh>#7</vh></v>
</v>
<v t="gcross.20091211100630.1307"><vh>@path sources</vh>
<v t="gcross.20091211140008.1289"><vh>@thin Models.hs</vh></v>
<v t="gcross.20091211100630.1380"><vh>@thin plot-gadget-state-difference.hs</vh></v>
<v t="gcross.20091211140008.1287"><vh>@thin plot-gadget-energy-gap.hs</vh></v>
<v t="gcross.20091215162646.1343"><vh>@thin sweep-bitslayer-gadget.hs</vh></v>
<v t="gcross.20091211140008.1719"><vh>@thin simulate-gadget.hs</vh></v>
</v>
</vnodes>
<tnodes>
<t tx="gcross.20091211100630.1232" str_leo_pos="3"></t>
<t tx="gcross.20091211100630.1307"></t>
<t tx="gcross.20091211140008.1277"></t>
<t tx="gcross.20091211140008.1278">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,6),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,5] = 0

middle_operator_site_tensor = zeros((2,2,6,6),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I
middle_operator_site_tensor[...,4,4] = I
middle_operator_site_tensor[...,4,5] = Z
middle_operator_site_tensor[...,5,5] = I

right_operator_site_tensor = zeros((2,2,6,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -perturbation_coefficient*X/2
right_operator_site_tensor[...,4,0] = 0
right_operator_site_tensor[...,5,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]

</t>
<t tx="gcross.20091211140008.1279">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,12),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,12,12),complex128)
middle_operator_site_tensor[..., 0, 0] = I
middle_operator_site_tensor[..., 0, 1] = X
middle_operator_site_tensor[..., 1, 1] = I
middle_operator_site_tensor[..., 2, 2] = I
middle_operator_site_tensor[..., 2, 3] = X
middle_operator_site_tensor[..., 3, 3] = I
middle_operator_site_tensor[..., 4, 7] = I
middle_operator_site_tensor[..., 7, 4] = I
middle_operator_site_tensor[..., 4, 5] = -Z
middle_operator_site_tensor[..., 5, 6] = Z
middle_operator_site_tensor[..., 6, 6] = I
middle_operator_site_tensor[..., 8,11] = I
middle_operator_site_tensor[...,11, 8] = I
middle_operator_site_tensor[..., 8, 9] = -X
middle_operator_site_tensor[..., 9,10] = X
middle_operator_site_tensor[...,10,10] = I

right_operator_site_tensor = zeros((2,2,12,1),complex128)
right_operator_site_tensor[..., 1, 0] = I
right_operator_site_tensor[..., 3, 0] = -perturbation_coefficient*X/2
right_operator_site_tensor[..., 4, 0] = 0
right_operator_site_tensor[..., 6, 0] = I
right_operator_site_tensor[...,10, 0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
</t>
<t tx="gcross.20091211140008.1280">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,12),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,12,12),complex128)
middle_operator_site_tensor[..., 0, 0] = I
middle_operator_site_tensor[..., 0, 1] = X
middle_operator_site_tensor[..., 1, 1] = I
middle_operator_site_tensor[..., 2, 2] = I
middle_operator_site_tensor[..., 2, 3] = X
middle_operator_site_tensor[..., 3, 3] = I
middle_operator_site_tensor[..., 4, 7] = I
middle_operator_site_tensor[..., 7, 4] = I
middle_operator_site_tensor[..., 4, 5] = -Z
middle_operator_site_tensor[..., 5, 6] = Z
middle_operator_site_tensor[..., 8,11] = I
middle_operator_site_tensor[...,11, 8] = I
middle_operator_site_tensor[..., 8, 9] = -Y
middle_operator_site_tensor[..., 9,10] = Y

right_operator_site_tensor = zeros((2,2,12,1),complex128)
right_operator_site_tensor[..., 1, 0] = I
right_operator_site_tensor[..., 3, 0] = -perturbation_coefficient*X/2
right_operator_site_tensor[..., 4, 0] = 0
right_operator_site_tensor[..., 6, 0] = I
right_operator_site_tensor[...,10, 0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]

</t>
<t tx="gcross.20091211140008.1281">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,12),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,12,12),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I
middle_operator_site_tensor[...,4,4] = I
middle_operator_site_tensor[...,4,5] = Z
middle_operator_site_tensor[...,5,5] = I
middle_operator_site_tensor[...,6,6] = I
middle_operator_site_tensor[...,6,7] = -Z
middle_operator_site_tensor[...,7,8] = Z
middle_operator_site_tensor[...,8,8] = I

right_operator_site_tensor = zeros((2,2,12,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -perturbation_coefficient*X/2
right_operator_site_tensor[...,5,0] = I
right_operator_site_tensor[...,8,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
</t>
<t tx="gcross.20091211140008.1282">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,12),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,12,12),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I
middle_operator_site_tensor[...,4,4] = I
middle_operator_site_tensor[...,4,5] = Z
middle_operator_site_tensor[...,5,5] = I
middle_operator_site_tensor[...,6,6] = I
middle_operator_site_tensor[...,6,7] = -Y
middle_operator_site_tensor[...,7,8] = Y
middle_operator_site_tensor[...,8,8] = I

right_operator_site_tensor = zeros((2,2,12,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -perturbation_coefficient*X/2
right_operator_site_tensor[...,5,0] = I
right_operator_site_tensor[...,8,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
</t>
<t tx="gcross.20091211140008.1283">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,12),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,12,12),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I

right_operator_site_tensor = zeros((2,2,12,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -perturbation_coefficient*X/2
right_operator_site_tensor[...,5,0] = I
right_operator_site_tensor[...,8,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
</t>
<t tx="gcross.20091211140008.1284">@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,14),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,13] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,14,14),complex128)
middle_operator_site_tensor[..., 0,12] = I
middle_operator_site_tensor[...,12, 0] = I
middle_operator_site_tensor[..., 0, 1] = X
middle_operator_site_tensor[..., 1, 1] = I
middle_operator_site_tensor[..., 2,13] = I
middle_operator_site_tensor[...,13, 2] = I
middle_operator_site_tensor[..., 2, 3] = X
middle_operator_site_tensor[..., 3, 3] = I
middle_operator_site_tensor[..., 4, 7] = I
middle_operator_site_tensor[..., 7, 4] = I
middle_operator_site_tensor[..., 4, 5] = -Z
middle_operator_site_tensor[..., 5, 6] = Z
middle_operator_site_tensor[..., 8,11] = I
middle_operator_site_tensor[...,11, 8] = I
middle_operator_site_tensor[..., 8, 9] = -X
middle_operator_site_tensor[..., 9,10] = X

right_operator_site_tensor = zeros((2,2,14,1),complex128)
right_operator_site_tensor[..., 1, 0] = I
right_operator_site_tensor[..., 3, 0] = -perturbation_coefficient*X/2
right_operator_site_tensor[..., 4, 0] = 0
right_operator_site_tensor[..., 6, 0] = I
right_operator_site_tensor[...,10, 0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
</t>
</tnodes>
</leo_file>
