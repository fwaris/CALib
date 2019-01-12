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

let applyBg imageOpt (c:FSharp.Charting.ChartTypes.GenericChart) = 
    match imageOpt with
    | Some image ->
        c.ApplyToChart (fun c -> 
            let a = c.ChartAreas.[0]
            a.BackImage <- image
            a.BackImageWrapMode <- System.Windows.Forms.DataVisualization.Charting.ChartImageWrapMode.Scaled)
    | None -> c

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

let box (points:(float*float)seq) =
  if Seq.length points < 2 then []
  else
    let x1,y1 = points |> Seq.item 0
    let x2,y2 = points |> Seq.item 1
    [x1,y1;x2,y1;x2,y2;x1,y2;x1,y1]

let chPoints2 bg title obs =
    let obs1,obs2 = obs |> Observable.separate
    let ch =
        [
          LiveChart.FastPoint(obs1); 
          LiveChart.FastPoint(obs2) 
          |> Chart.WithSeries.Marker(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)
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
            |> Chart.WithSeries.Marker(Size=10)
        )
      |> Chart.Combine 
      |> Chart.WithTitle title
      |> Chart.WithLegend(Enabled=true)
      |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
      |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
      |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
      |> applyBg bg
    ch,bg

let chPtsLine bg title obs =
    let obs1,obs2 = obs |> Observable.separate
    let obsB = obs2 |> Observable.map box
    let ch =
        [
          LiveChart.FastPoint(obs1); 
          LiveChart.FastLine(obsB) 
          |> Chart.WithSeries.Marker(Size=10, Color=Color.Transparent, BorderColor=Color.DarkBlue, BorderWidth=2)
        ]
        |> Chart.Combine 
        |> Chart.WithTitle title
        |> Chart.WithTitle(Color=System.Drawing.Color.DarkBlue)
        |> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid, LabelStyle=ls)
        |> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=chGrid)
        |> applyBg bg
    ch,bg

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
    ch,(None:string option)

let chDisp title obs =  LiveChart.FastLineIncremental(obs,Title=title),(None:string option)

let containerize (ch,bg) = 
    let chh = new ChartTypes.ChartControl(ch,Dock=DockStyle.Fill)
    match bg with
    | Some image -> applyBgHost image chh
    | None       -> ()
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
    let containers = chlist |> List.map containerize 
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
