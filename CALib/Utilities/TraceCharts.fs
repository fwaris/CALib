﻿///Vizualiation of CA run with charts (animated in some cases)
module TraceCharts
open FSharp.Charting
open System.Windows.Forms.DataVisualization
open System.Windows.Forms
open System.Drawing
open System

let applyBgHost image (chh:ChartTypes.ChartControl) = 
    let ch = chh.Controls.[0] :?> System.Windows.Forms.DataVisualization.Charting.Chart
    ch.ChartAreas.[0].BackImage <- image
    ch.ChartAreas.[0].BackImageWrapMode <- System.Windows.Forms.DataVisualization.Charting.ChartImageWrapMode.Scaled

let applyBgChart imageOpt (c:Charting.Chart) = 
    match imageOpt with
    | Some image ->
        let a = c.ChartAreas.[0]
        a.BackImage <- image
        a.BackImageWrapMode <- System.Windows.Forms.DataVisualization.Charting.ChartImageWrapMode.Scaled
        match c.Parent with :? ChartTypes.ChartControl as h -> applyBgHost image h | _ -> ()
    | None -> ()

let applyBg imageOpt (c:FSharp.Charting.ChartTypes.GenericChart) = c.ApplyToChart (applyBgChart imageOpt)

let withBgSubscription obs (c:FSharp.Charting.ChartTypes.GenericChart)  =
    let refCh = ref None
    let _ = obs |> Observable.subscribe (fun imgFile -> 
        printfn "refCh changed %A" !refCh
        match !refCh with
        | Some c -> applyBgChart imgFile c 
        | None  ->())
    c.ApplyToChart(fun c -> 
        printfn "refCH"
        refCh := Some c) |> ignore
    c

let chGrid = ChartTypes.Grid(Enabled=false,Interval=0.1)
let ls = ChartTypes.LabelStyle(TruncatedLabels=true, Interval=0.2, Format="{0:F1}")

let round' x = System.Math.Round((x:float),2)

let chPoints bg title obs =
    let ch =
        LiveChart.FastPoint(obs , Title=title) 
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
        |> applyBg bg
    ch,bg

let chPointsObs title bgObs obs =
    let ch =
        LiveChart.FastPoint(obs , Title=title) 
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
    ch,bgObs

let box (points:(float*float)seq) =
  if Seq.length points < 2 then []
  else
    let x1,y1 = points |> Seq.item 0
    let x2,y2 = points |> Seq.item 1
    [x1,y1;x2,y1;x2,y2;x1,y2;x1,y1]


let chPoints2Obs title bgObs obs =
    let obs1,obs2 = obs |> Observable.separate

    let ch =
        [
          LiveChart.FastPoint(obs1); 
          LiveChart.FastPoint(obs2) 
          |> Chart.WithMarkers(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)

        ]
        |> Chart.Combine 
        |> Chart.WithTitle title
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
    ch,bgObs

let chPoints2 bg title obs =
    let obs1,obs2 = obs |> Observable.separate
    let ch =
        [
          LiveChart.FastPoint(obs1); 
          LiveChart.FastPoint(obs2) 
          |> Chart.WithMarkers(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)
        ]
        |> Chart.Combine 
        |> Chart.WithTitle title
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
        |> applyBg bg
    ch,bg

let chPointsN bg title obss =
    let ch =
      obss
      |> List.map (fun (t,obs) -> 
            LiveChart.FastPoint(obs)
            |> Chart.WithStyling(Name=t)
            |> Chart.WithMarkers(Size=10)
        )
      |> Chart.Combine 
      |> Chart.WithTitle title
      |> Chart.WithLegend(Enabled=true)
      |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
      |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
      |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
      |> applyBg bg
    ch,bg

let chPointsNObs title bgObs obss =
    let ch =
      obss
      |> List.map (fun (t,obs) -> 
            LiveChart.FastPoint(obs)
            |> Chart.WithStyling(Name=t)
            |> Chart.WithMarkers(Size=10)
        )
      |> Chart.Combine 
      |> Chart.WithTitle title
      |> Chart.WithLegend(Enabled=true)
      |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
      |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
      |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
    ch,bgObs

let chPointsNwBestObs title bgObs obss (tbst,obBst) =
    let ch =
      seq {
        for (t,obs) in obss do
            yield 
                LiveChart.FastPoint(obs)
                |> Chart.WithStyling(Name=t)
                |> Chart.WithMarkers(Size=10)
        yield
            LiveChart.FastPoint(obBst)
            |> Chart.WithStyling(Name=tbst)
            |> Chart.WithMarkers(Size=20,Color=Color.Transparent, BorderColor=Color.IndianRed, BorderWidth=3, Style=ChartTypes.MarkerStyle.Star4)
      } 
      |> Chart.Combine 
      |> Chart.WithTitle title
      |> Chart.WithLegend(Enabled=true)
      |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
      |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
      |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
    ch,bgObs

let chPtsLine bg title obs =
    let obs1,obs2 = obs |> Observable.separate
    let obsB = obs2 |> Observable.map box
    let ch =
        [
          LiveChart.FastPoint(obs1); 
          LiveChart.FastLine(obsB) 
          |> Chart.WithMarkers(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)
        ]
        |> Chart.Combine 
        |> Chart.WithTitle title
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
        |> applyBg bg
    ch,bg

let chPtsLineObs title bgObs obs =
    let obs1,obs2 = obs |> Observable.separate
    let obsB = obs2 |> Observable.map box
    let ch =
        [
          LiveChart.FastPoint(obs1); 
          LiveChart.FastLine(obsB) 
          |> Chart.WithMarkers(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)
        ]
        |> Chart.Combine 
        |> Chart.WithTitle title
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
    ch,bgObs

let chLines (mn,mx) title obs = 
  let ch = 
    obs 
    |> Seq.map (fun o -> LiveChart.FastLineIncremental o) 
    |> Chart.Combine
    |> Chart.WithYAxis(Max=mx, Min=mn)
  ch |> Chart.WithTitle title ,None


let chCounts obs =
    let ch =
        LiveChart.Column(obs, Title="Live KS Counts") 
        |> Chart.WithStyling(Color=System.Drawing.Color.Tomato)
    ch,None

let chDisp title obs =  LiveChart.FastLineIncremental(obs,Title=title),(None:string option)

let containerize (ch,bg) = 
    let chh = new ChartTypes.ChartControl(ch,Dock=DockStyle.Fill)
    let x = chh.Controls
    match bg with
    | Some image -> applyBgHost image chh
    | None       -> ()
    chh

let containerizeWithBg (ch,obs) = 
    let chh = new ChartTypes.ChartControl(ch,Dock=DockStyle.Fill)
    obs |> Option.iter(fun obs -> 
        let c = chh.Controls.[0] :?> Charting.Chart
        obs |> Observable.subscribe (fun image -> applyBgChart image c) |> ignore)
    chh

let chOne ch = 
  let ch = containerize ch
  let frm = new System.Windows.Forms.Form()
  frm.Controls.Add(ch)
  frm.Show()
  frm

let container chlist =
    let form = new Form()
    form.Width  <- 400
    form.Height <- 600
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 3
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 33.f)) |> ignore
    grid.RowCount <- 2
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    let containers = chlist |> List.map containerizeWithBg 
    containers |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
    form

let container2Row chlist =
    let form = new Form()
    form.Width  <- 400
    form.Height <- 600
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 1
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100.f)) |> ignore
    grid.RowCount <- 2
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,75.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,25.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    let containers = chlist |> List.map containerizeWithBg 
    containers |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
    form

let container1Row chlist =
    let form = new Form()
    form.Width  <- 400
    form.Height <- 600
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 1
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100.f)) |> ignore
    grid.RowCount <- 1
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,100.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    let containers = chlist |> List.map containerizeWithBg 
    containers |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
    form

let container3Row chlist =
    let form = new Form()
    form.Width  <- 400
    form.Height <- 600
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 1
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100.f)) |> ignore
    grid.RowCount <- 3
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,50.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,25.f)) |> ignore
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,25.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    let containers = chlist |> List.map containerizeWithBg 
    containers |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
    form

let container2Col chlist =
    let form = new Form()
    form.Width  <- 600
    form.Height <- 300
    form.Visible <- true 
    form.Text <- "CA Charts"
    let grid = new TableLayoutPanel()
    grid.AutoSize <- true
    grid.ColumnCount <- 2
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 50.f)) |> ignore
    grid.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 50.f)) |> ignore
    grid.RowCount <- 1
    grid.RowStyles.Add(new RowStyle(SizeType.Percent,100.f)) |> ignore
    grid.GrowStyle <-  TableLayoutPanelGrowStyle.AddRows
    grid.Dock <- DockStyle.Fill
    let containers = chlist |> List.map containerizeWithBg 
    containers |> List.iter grid.Controls.Add
    form.Controls.Add(grid)
    form.Show()
    form

    //formList.zip chlist containers

let rec updateBg chlist newImage =
    chlist |> List.iter (fun ((chart,oldIm),control) ->
        match oldIm with
        | Some _ -> 
            applyBg (Some newImage) chart |> ignore
            applyBgHost newImage control
        | None -> ())

let rec updateBgForm (parent:Control) newImage =
    for c in parent.Controls do
        match c with 
        | :? ChartTypes.ChartControl as chh ->
            let c = chh.Controls.[0] :?> DataVisualization.Charting.Chart
            if System.String.IsNullOrWhiteSpace (c.ChartAreas.[0].BackImage) |> not then
                //applyBgHost newImage chh 
                let a = c.ChartAreas.[0]
                a.BackImage <- newImage
        | c -> if c.HasChildren then updateBgForm c newImage
